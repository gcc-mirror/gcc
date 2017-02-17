/* Test _Float64x <float.h> macros.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float64x } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float64x_runtime } */

#define WIDTH 64
#define EXT 1
#include "floatn-floath.h"

#ifndef FLT64X_MANT_DIG
# error "FLT64X_MANT_DIG undefined"
#endif

#ifndef FLT64X_DECIMAL_DIG
# error "FLT64X_DECIMAL_DIG undefined"
#endif

#ifndef FLT64X_DIG
# error "FLT64X_DIG undefined"
#endif

#ifndef FLT64X_MIN_EXP
# error "FLT64X_MIN_EXP undefined"
#endif

#ifndef FLT64X_MIN_10_EXP
# error "FLT64X_MIN_10_EXP undefined"
#endif

#ifndef FLT64X_MAX_EXP
# error "FLT64X_MAX_EXP undefined"
#endif

#ifndef FLT64X_MAX_10_EXP
# error "FLT64X_MAX_10_EXP undefined"
#endif

#ifndef FLT64X_MAX
# error "FLT64X_MAX undefined"
#endif

#ifndef FLT64X_EPSILON
# error "FLT64X_EPSILON undefined"
#endif

#ifndef FLT64X_MIN
# error "FLT64X_MIN undefined"
#endif

#ifndef FLT64X_TRUE_MIN
# error "FLT64X_TRUE_MIN undefined"
#endif

#if FLT64X_MANT_DIG < 64 || FLT64X_MAX_EXP < 16384 || FLT64X_MIN_EXP + FLT64X_MAX_EXP != 3
# error "_Float64x bad format"
#endif
