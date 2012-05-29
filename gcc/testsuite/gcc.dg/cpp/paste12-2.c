/* 
   { dg-options "-ftrack-macro-expansion=2" }
   { dg-do preprocess }
 */

/* Test correct diagnostics when pasting in #include.
   Source: PR preprocessor/6780.  */

#define inc2(a,b) <##a.b>  /* { dg-error "pasting \"<\" and \"stdio\" does not" } */
#define INC(X) inc2(X,h)
#include INC(stdio)
