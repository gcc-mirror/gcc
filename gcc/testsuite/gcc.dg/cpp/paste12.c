/* { dg-do preprocess } */

/* Test correct diagnostics when pasting in #include.
   Source: PR preprocessor/6780.  */

#define inc2(a,b) <##a.b>
#define INC(X) inc2(X,h)
#include INC(stdio) /* { dg-error "pasting \"<\" and \"stdio\" does not" } */
