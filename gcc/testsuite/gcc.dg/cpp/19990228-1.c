/* Regression test for cpp.  The following input may cause core dumps
   or # line markers in the middle of the line.  */
/* { dg-do preprocess } */

#define foo(string, arg) bar(2, string, arg)

foo ("\
\
\
\
\
\
\
\
\
\
\
",
NULL);

/* { dg-final { scan-file-not 19990228-1.i "\[^\\n\]#" } } */
