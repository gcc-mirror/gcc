/* Test _Float128 <float.h> macros.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float128 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float128_runtime } */

#define WIDTH 128
#define EXT 0
#include "floatn-floath.h"

#ifndef FLT128_MANT_DIG
# error "FLT128_MANT_DIG undefined"
#endif

#ifndef FLT128_DECIMAL_DIG
# error "FLT128_DECIMAL_DIG undefined"
#endif

#ifndef FLT128_DIG
# error "FLT128_DIG undefined"
#endif

#ifndef FLT128_MIN_EXP
# error "FLT128_MIN_EXP undefined"
#endif

#ifndef FLT128_MIN_10_EXP
# error "FLT128_MIN_10_EXP undefined"
#endif

#ifndef FLT128_MAX_EXP
# error "FLT128_MAX_EXP undefined"
#endif

#ifndef FLT128_MAX_10_EXP
# error "FLT128_MAX_10_EXP undefined"
#endif

#ifndef FLT128_MAX
# error "FLT128_MAX undefined"
#endif

#ifndef FLT128_EPSILON
# error "FLT128_EPSILON undefined"
#endif

#ifndef FLT128_MIN
# error "FLT128_MIN undefined"
#endif

#ifndef FLT128_TRUE_MIN
# error "FLT128_TRUE_MIN undefined"
#endif

#if FLT128_MANT_DIG != 113 || FLT128_MAX_EXP != 16384 || FLT128_MIN_EXP != -16381
# error "_Float128 bad format"
#endif
