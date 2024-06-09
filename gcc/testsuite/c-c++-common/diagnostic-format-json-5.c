/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=json" } */

struct s { int color; };

int test (struct s *ptr)
{
  return ptr->colour;
}

/* { dg-begin-multiline-output "" }
[{"kind": "error",
  "message": "'struct s' has no member named 'colour'; did you mean 'color'?",
  "children": [],
  "column-origin": 1,
  "locations": [{"caret": {"file":
                           "line": 8,
                           "display-column": 15,
                           "byte-column": 15,
                           "column": 15},
                 "finish": {"file":
                            "line": 8,
                            "display-column": 20,
                            "byte-column": 20,
                            "column": 20}}],
  "fixits": [{"start": {"file":
                        "line": 8,
                        "display-column": 15,
                        "byte-column": 15,
                        "column": 15},
              "next": {"file":
                       "line": 8,
                       "display-column": 21,
                       "byte-column": 21,
                       "column": 21},
              "string": "color"}],
  "escape-source": false}]
   { dg-end-multiline-output "" } */
