/* { dg-do preprocess } */
/* { dg-options "-CC -traditional-cpp" } */

/* This tests to make sure that comments between the #define directive
   and the macro identifier are ignored (i.e. treated like whitespace)
   when the -CC option is used.

   Jason R. Thorpe, 6 Apr 2002  */

#define/**/def passed

def

/* { dg-final { scan-file maccom2.i "(^|\n)passed" } } */
