/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=json -Wmisleading-indentation" } */

int test (void)
{
  if (1)
    return 3;
    return 4;
  return 5;
}

/* { dg-begin-multiline-output "" }
[{"kind": "warning",
  "message": "this 'if' clause does not guard...",
  "option": "-Wmisleading-indentation",
   { dg-end-multiline-output "" } */
/* { dg-regexp "  \"option_url\": \"https:\[^\n\r\"\]*#index-Wmisleading-indentation\",\n" } */
/* { dg-begin-multiline-output "" }
  "children": [{"kind": "note",
                "message": "...this statement, but the latter is misleadingly indented as if it were guarded by the 'if'",
                "locations": [{"caret": {"file":
                                         "line": 8,
                                         "display-column": 5,
                                         "byte-column": 5,
                                         "column": 5},
                               "finish": {"file":
                                          "line": 8,
                                          "display-column": 10,
                                          "byte-column": 10,
                                          "column": 10}}],
                "escape-source": false}],
  "column-origin": 1,
  "locations": [{"caret": {"file":
                           "line": 6,
                           "display-column": 3,
                           "byte-column": 3,
                           "column": 3},
                 "finish": {"file":
                            "line": 6,
                            "display-column": 4,
                            "byte-column": 4,
                            "column": 4}}],
  "escape-source": false}]
   { dg-end-multiline-output "" } */
