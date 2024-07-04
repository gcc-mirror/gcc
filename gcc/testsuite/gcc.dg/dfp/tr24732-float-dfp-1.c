/* Test DFP macros defined in <float.h> with DFP support.  TR 24732
   feature test macro causes SUBNORMAL_MIN macros to be defined but
   not TRUE_MIN ones.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#define __STDC_WANT_DEC_FP__
#include <float.h>

#ifndef DEC32_MANT_DIG
# error "DEC32_MANT_DIG not defined"
#endif

#ifndef DEC64_MANT_DIG
# error "DEC64_MANT_DIG not defined"
#endif

#ifndef DEC128_MANT_DIG
# error "DEC128_MANT_DIG not defined"
#endif

#ifndef DEC32_MIN_EXP
# error "DEC32_MIN_EXP not defined"
#endif

#ifndef DEC64_MIN_EXP
# error "DEC64_MIN_EXP not defined"
#endif

#ifndef DEC128_MIN_EXP
# error "DEC128_MIN_EXP not defined"
#endif

#ifndef DEC32_MAX_EXP
# error "DEC32_MAX_EXP not defined"
#endif

#ifndef DEC64_MAX_EXP
# error "DEC64_MAX_EXP not defined"
#endif

#ifndef DEC128_MAX_EXP
# error "DEC128_MAX_EXP not defined"
#endif

#ifndef DEC32_MAX
# error "DEC32_MAX not defined"
#endif

#ifndef DEC64_MAX
# error "DEC64_MAX not defined"
#endif

#ifndef DEC128_MAX
# error "DEC128_MAX not defined"
#endif

#ifndef DEC32_EPSILON
# error "DEC32_EPSILON not defined"
#endif

#ifndef DEC64_EPSILON
# error "DEC64_EPSILON not defined"
#endif

#ifndef DEC128_EPSILON
# error "DEC128_EPSILON not defined"
#endif

#ifndef DEC32_MIN
# error "DEC32_MIN not defined"
#endif

#ifndef DEC64_MIN
# error "DEC64_MIN not defined"
#endif

#ifndef DEC128_MIN
# error "DEC128_MIN not defined"
#endif

#ifndef DEC32_SUBNORMAL_MIN
# error "DEC32_SUBNORMAL_MIN not defined"
#endif

#ifndef DEC64_SUBNORMAL_MIN
# error "DEC64_SUBNORMAL_MIN not defined"
#endif

#ifndef DEC128_SUBNORMAL_MIN
# error "DEC128_SUBNORMAL_MIN not defined"
#endif

/* These macros from C23 should not be defined.  */

#ifdef DEC32_TRUE_MIN
# error "DEC32_TRUE_MIN defined"
#endif

#ifdef DEC64_TRUE_MIN
# error "DEC64_TRUE_MIN defined"
#endif

#ifdef DEC128_TRUE_MIN
# error "DEC128_TRUE_MIN defined"
#endif
