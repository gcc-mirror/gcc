/* Test DFP macros not defined in <float.h> if no DFP support.
   Infinity and NaN macros.  */
/* { dg-do compile { target { ! dfp } } } */
/* { dg-options "-std=c23" } */

#include <float.h>

#ifdef DEC_INFINITY
# error "DEC_INFINITY defined"
#endif

#ifdef DEC_NAN
# error "DEC_NAN defined"
#endif

#ifdef DEC32_SNAN
# error "DEC32_SNAN defined"
#endif

#ifdef DEC64_SNAN
# error "DEC64_SNAN defined"
#endif

#ifdef DEC128_SNAN
# error "DEC128_SNAN defined"
#endif
