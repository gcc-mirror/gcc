! { dg-do compile }
! { dg-options "-fdiagnostics-format=json -Werror" }

#warning message

#if 0
{ dg-begin-multiline-output "" }
[{"kind": "error",
  "message": "#warning message",
  "option": "-Werror=cpp",
  "option_url":
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
{ dg-end-multiline-output "" }
#endif  
