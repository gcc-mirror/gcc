/* { dg-do preprocess } */
/* { dg-options "-C -P" } */

#define macro(X) X

macro(
// Comment1
x
// Comment2
);

/* { dg-final { scan-file cmdlne-C3.i "\\\*\\\/ x \\\/\\\*" } } */

