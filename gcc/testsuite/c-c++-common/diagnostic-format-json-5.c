/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=json" } */

struct s { int color; };

int test (struct s *ptr)
{
  return ptr->colour;
}

/* Verify fix-it hints.

   Use dg-regexp to consume the JSON output from start to
   finish, relying on the ordering of the keys.
   The following uses indentation to visualize the structure
   of the JSON (although the actual output is all on one line).

  { dg-regexp {\[} }
    { dg-regexp {\{} }
     { dg-regexp {"kind": "error"} }
     { dg-regexp {, "message": "'struct s' has no member named 'colour'; did you mean 'color'\?"} }
     { dg-regexp {, "children": \[\]} }
     { dg-regexp {, "column-origin": 1} }
     { dg-regexp {, "locations": } }
       { dg-regexp {\[} }
         { dg-regexp {\{} }
           { dg-regexp {"caret": } }
             { dg-regexp {\{} }
               { dg-regexp {"file": "[^\n\r"]*diagnostic-format-json-5.c"} }
               { dg-regexp {, "line": 8} }
               { dg-regexp {, "display-column": 15} }
               { dg-regexp {, "byte-column": 15} }
               { dg-regexp {, "column": 15} }
             { dg-regexp {\}} }
           { dg-regexp {, "finish": } }
             { dg-regexp {\{} }
               { dg-regexp {"file": "[^\n\r"]*diagnostic-format-json-5.c"} }
               { dg-regexp {, "line": 8} }
               { dg-regexp {, "display-column": 20} }
               { dg-regexp {, "byte-column": 20} }
               { dg-regexp {, "column": 20} }
             { dg-regexp {\}} }
           { dg-regexp {\}} }
         { dg-regexp {\]} }
       { dg-regexp {, "fixits": } }
       { dg-regexp {\[} }
         { dg-regexp {\{} }
           { dg-regexp {"start": } }
             { dg-regexp {\{} }
               { dg-regexp {"file": "[^\n\r"]*diagnostic-format-json-5.c"} }
               { dg-regexp {, "line": 8} }
               { dg-regexp {, "display-column": 15} }
               { dg-regexp {, "byte-column": 15} }
               { dg-regexp {, "column": 15} }
             { dg-regexp {\}} }
           { dg-regexp {, "next": } }
             { dg-regexp {\{} }
               { dg-regexp {"file": "[^\n\r"]*diagnostic-format-json-5.c"} }
               { dg-regexp {, "line": 8} }
               { dg-regexp {, "display-column": 21} }
               { dg-regexp {, "byte-column": 21} }
               { dg-regexp {, "column": 21} }
             { dg-regexp {\}} }
           { dg-regexp {, "string": "color"} }
         { dg-regexp {\}} }
       { dg-regexp {\]} }
     { dg-regexp {, "escape-source": false\}} }
   { dg-regexp {\]} }  */
