/* Test DFP macros not defined in <float.h> if no DFP support.
   Infinity and NaN macros.  */
/* { dg-do compile { target { ! dfp } } } */
/* { dg-options "-std=c2x" } */

#include <float.h>

#ifdef DEC_INFINITY
# error "DEC_INFINITY defined"
#endif

#ifdef DEC_NAN
# error "DEC_NAN defined"
#endif
