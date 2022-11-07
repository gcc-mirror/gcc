/* Test unreachable not defined in <stddef.h> for C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stddef.h>

#ifdef unreachable
#error "unreachable defined"
#endif
