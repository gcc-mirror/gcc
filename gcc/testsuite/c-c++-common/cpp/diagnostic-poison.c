/* PR preprocessor/36887 */
/* { dg-do preprocess } */

#ifdef LEVEL2
/* Test that we get the include traced location as well.  */
#pragma GCC poison p1 /* { dg-note "poisoned here" } */
#else
#define LEVEL2
#include "diagnostic-poison.c"
int p1; /* { dg-error "attempt to use poisoned" } */
_Pragma("GCC poison p2") /* { dg-note "poisoned here" } */
int p2; /* { dg-error "attempt to use poisoned" } */
#endif
