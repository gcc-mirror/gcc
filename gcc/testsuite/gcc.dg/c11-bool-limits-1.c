/* Test limits for _Bool not in <limits.h> in C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11" } */

#include <limits.h>

#ifdef BOOL_MAX
# error "unexpected BOOL_MAX"
#endif

#ifdef BOOL_WIDTH
# error "unexpected BOOL_WIDTH"
#endif
