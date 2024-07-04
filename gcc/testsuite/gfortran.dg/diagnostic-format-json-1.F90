! { dg-do compile }
! { dg-options "-fdiagnostics-format=json" }

#error message

#if 0
{ dg-begin-multiline-output "" }
[{"kind": "error",
  "message": "#error message",
  "children": [],
  "column-origin": 1,
  "locations": [{"caret": {"file":
                           "line": 4,
                           "display-column": 2,
                           "byte-column": 2,
                           "column": 2},
                 "finish": {"file":
                            "line": 4,
                            "display-column": 6,
                            "byte-column": 6,
                            "column": 6}}],
  "escape-source": false}]
{ dg-end-multiline-output "" }
#endif  
