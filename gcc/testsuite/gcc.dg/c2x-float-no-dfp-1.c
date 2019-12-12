/* Test DFP macros not defined in <float.h> if no DFP support.  */
/* { dg-do compile { target { ! dfp } } } */
/* { dg-options "-std=c2x" } */

#include <float.h>

#ifdef DEC32_MANT_DIG
# error "DEC32_MANT_DIG defined"
#endif

#ifdef DEC64_MANT_DIG
# error "DEC64_MANT_DIG defined"
#endif

#ifdef DEC128_MANT_DIG
# error "DEC128_MANT_DIG defined"
#endif

#ifdef DEC32_MIN_EXP
# error "DEC32_MIN_EXP defined"
#endif

#ifdef DEC64_MIN_EXP
# error "DEC64_MIN_EXP defined"
#endif

#ifdef DEC128_MIN_EXP
# error "DEC128_MIN_EXP defined"
#endif

#ifdef DEC32_MAX_EXP
# error "DEC32_MAX_EXP defined"
#endif

#ifdef DEC64_MAX_EXP
# error "DEC64_MAX_EXP defined"
#endif

#ifdef DEC128_MAX_EXP
# error "DEC128_MAX_EXP defined"
#endif

#ifdef DEC32_MAX
# error "DEC32_MAX defined"
#endif

#ifdef DEC64_MAX
# error "DEC64_MAX defined"
#endif

#ifdef DEC128_MAX
# error "DEC128_MAX defined"
#endif

#ifdef DEC32_EPSILON
# error "DEC32_EPSILON defined"
#endif

#ifdef DEC64_EPSILON
# error "DEC64_EPSILON defined"
#endif

#ifdef DEC128_EPSILON
# error "DEC128_EPSILON defined"
#endif

#ifdef DEC32_MIN
# error "DEC32_MIN defined"
#endif

#ifdef DEC64_MIN
# error "DEC64_MIN defined"
#endif

#ifdef DEC128_MIN
# error "DEC128_MIN defined"
#endif

#ifdef DEC32_TRUE_MIN
# error "DEC32_TRUE_MIN defined"
#endif

#ifdef DEC64_TRUE_MIN
# error "DEC64_TRUE_MIN defined"
#endif

#ifdef DEC128_TRUE_MIN
# error "DEC128_TRUE_MIN defined"
#endif

#ifdef DEC32_SUBNORMAL_MIN
# error "DEC32_SUBNORMAL_MIN defined"
#endif

#ifdef DEC64_SUBNORMAL_MIN
# error "DEC64_SUBNORMAL_MIN defined"
#endif

#ifdef DEC128_SUBNORMAL_MIN
# error "DEC128_SUBNORMAL_MIN defined"
#endif
