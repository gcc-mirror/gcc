/* This is padding (to avoid the output containing DejaGnu directives).  */

/* We need -fdiagnostics-show-caret to trigger the ICE.  */

/* { dg-options "-fdiagnostics-show-caret -pedantic-errors -Wno-implicit-function-declaration" } */

#line 3482810481 /* { dg-error "line number out of range" } */
/* { dg-begin-multiline-output "" }
 #line 3482810481
       ^~~~~~~~~~
   { dg-end-multiline-output "" } */

int foo (void) { return strlen(""); }

/* { dg-warning "incompatible implicit declaration of built-in function 'strlen'" "" { target *-*-* } { -812156810 } } */
/* { dg-message "include '<string.h>' or provide a declaration of 'strlen'" "" { target *-*-* } 1 } */
#if 0
{ dg-begin-multiline-output "" }
+#include <string.h>
 /* This is padding (to avoid the output containing DejaGnu directives).  */
{ dg-end-multiline-output "" }
#endif
