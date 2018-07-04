module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, targetValue)
import Http
import Http.Progress as Progress exposing (Progress(..))
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd a )
init =
    ( initModel, Cmd.none )



-- Model


type alias Model =
    { progress : Progress String
    , bookUrl : Maybe String
    , bookContent : String
    }


initModel : Model
initModel =
    { progress = Progress.None
    , bookUrl = Nothing
    , bookContent = ""
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.bookUrl of
        Just bookUrl ->
            Http.getString bookUrl
                |> Progress.track bookUrl GetBookProgress

        Nothing ->
            Sub.none



-- Update


type Msg
    = NoOp
    | GetBook String
    | GetBookProgress (Progress String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetBook url ->
            ( { model | bookUrl = Just url }, Cmd.none )

        GetBookProgress (Done bookContent) ->
            ( { model
                | progress = Done ""
                , bookContent = bookContent
              }
            , Cmd.none
            )

        GetBookProgress (Fail error) ->
            ( { model
                | progress = Fail error
                , bookContent = toString error
              }
            , Cmd.none
            )

        GetBookProgress progress ->
            ( { model | progress = progress }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Elm Http.Progress example" ]
        , p []
            [ text "To better see the progress, "
            , a
                [ href "https://developers.google.com/web/tools/chrome-devtools/network-performance/#emulate" ]
                [ text "throttle your internet connection" ]
            , text "."
            ]
        , div [ class "select-content" ]
            [ select
                [ class "flex-grow-1"
                , on "change" (Decode.map GetBook targetValue)
                ]
                [ option
                    [ hidden True
                    , attribute "value" ""
                    ]
                    [ text "Select a book" ]
                , option
                    [ value "books/essays.txt" ]
                    [ text "Essays - Ralph Waldo Emerson" ]
                , option
                    [ value "books/leviathan.txt" ]
                    [ text "Leviathan - Thomas Hobbes" ]
                , option
                    [ value "books/ethics.txt" ]
                    [ text "The Ethics of Aristotle - Aristotle" ]
                ]
            , progressView (toString (progressLoaded model.progress))
            ]
        , bookContentView model.bookContent
        , footerView
        ]


progressView : String -> Html Msg
progressView loaded =
    div [ class "flex-grow-1" ]
        [ span [] [ text "Progress: " ]
        , progress
            [ value loaded
            , Html.Attributes.max "100"
            ]
            [ text <| loaded ++ "%" ]
        , text <| loaded ++ "%"
        ]


progressLoaded : Progress String -> Int
progressLoaded progress =
    case progress of
        Some { bytes, bytesExpected } ->
            round <|
                (*) 100 <|
                    toFloat bytes
                        / toFloat bytesExpected

        Done _ ->
            100

        _ ->
            -- None or Fail case
            0


bookContentView : String -> Html Msg
bookContentView valueText =
    textarea
        [ class "book-content"
        , value valueText
        , disabled True
        ]
        []


footerView : Html Msg
footerView =
    p []
        [ a
            [ href "https://github.com/pablohirafuji/elm-http-progress-example"
            , target "_blank"
            ]
            [ text "Source" ]
        , text " | Books from "
        , a
            [ href "http://www.gutenberg.org/"
            , target "_blank"
            ]
            [ text "Project Gutenberg" ]
        ]
