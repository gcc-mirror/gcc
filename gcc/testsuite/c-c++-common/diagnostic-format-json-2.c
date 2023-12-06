/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=json" } */

#warning message

/* { dg-begin-multiline-output "" }
[{"kind": "warning",
  "message": "#warning message",
  "option": "-Wcpp",
   { dg-end-multiline-output "" } */
/* { dg-regexp "  \"option_url\": \"https:\[^\n\r\"\]*#index-Wcpp\",\n" } */
/* { dg-begin-multiline-output "" }
  "children": [],
  "column-origin": 1,
  "locations": [{"caret": {"file":
                           "line": 4,
                           "display-column": 2,
                           "byte-column": 2,
                           "column": 2},
                 "finish": {"file":
                            "line": 4,
                            "display-column": 8,
                            "byte-column": 8,
                            "column": 8}}],
  "escape-source": false}]
   { dg-end-multiline-output "" } */
