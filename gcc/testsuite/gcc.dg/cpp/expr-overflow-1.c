/* Test overflow in preprocessor arithmetic.  PR 55715.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c99" } */

#include <stdint.h>

#if -1 - INTMAX_MIN
#endif

#if 0 - INTMAX_MIN /* { dg-warning "overflow" } */
#endif

#if 1 * INTMAX_MIN
#endif

#if -1 * INTMAX_MIN /* { dg-warning "overflow" } */
#endif

#if 0 * INTMAX_MIN
#endif

#if -INTMAX_MIN /* { dg-warning "overflow" } */
#endif

#if +INTMAX_MIN
#endif

#if INTMAX_MIN / 1
#endif

#if INTMAX_MIN / -1 /* { dg-warning "overflow" } */
#endif

#if UINTMAX_MAX * UINTMAX_MAX
#endif

#if UINTMAX_MAX / -1
#endif

#if UINTMAX_MAX + INTMAX_MAX
#endif

#if UINTMAX_MAX - INTMAX_MIN
#endif
