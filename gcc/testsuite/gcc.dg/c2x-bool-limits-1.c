/* Test limits for _Bool in <limits.h> in C2x.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x" } */

#include <limits.h>

#ifndef BOOL_MAX
# error "missing BOOL_MAX"
#endif

#ifndef BOOL_WIDTH
# error "missing BOOL_WIDTH"
#endif

/* In principle _Bool can support values wider than 1 bit, stored via
   type punning, but this is not supported by GCC.  */

_Static_assert (BOOL_MAX == 1, "bad BOOL_MAX");
_Static_assert (BOOL_WIDTH == 1, "bad BOOL_WIDTH");
