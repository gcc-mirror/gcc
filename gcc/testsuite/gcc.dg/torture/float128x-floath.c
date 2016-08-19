/* Test _Float128x <float.h> macros.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float128x } */
/* { dg-require-effective-target float128x_runtime } */

#define WIDTH 128
#define EXT 1
#include "floatn-floath.h"

#ifndef FLT128X_MANT_DIG
# error "FLT128X_MANT_DIG undefined"
#endif

#ifndef FLT128X_DECIMAL_DIG
# error "FLT128X_DECIMAL_DIG undefined"
#endif

#ifndef FLT128X_DIG
# error "FLT128X_DIG undefined"
#endif

#ifndef FLT128X_MIN_EXP
# error "FLT128X_MIN_EXP undefined"
#endif

#ifndef FLT128X_MIN_10_EXP
# error "FLT128X_MIN_10_EXP undefined"
#endif

#ifndef FLT128X_MAX_EXP
# error "FLT128X_MAX_EXP undefined"
#endif

#ifndef FLT128X_MAX_10_EXP
# error "FLT128X_MAX_10_EXP undefined"
#endif

#ifndef FLT128X_MAX
# error "FLT128X_MAX undefined"
#endif

#ifndef FLT128X_EPSILON
# error "FLT128X_EPSILON undefined"
#endif

#ifndef FLT128X_MIN
# error "FLT128X_MIN undefined"
#endif

#ifndef FLT128X_TRUE_MIN
# error "FLT128X_TRUE_MIN undefined"
#endif

#if FLT128X_DECIMAL_DIG > DECIMAL_DIG
# error "FLT128X_DECIMAL_DIG > DECIMAL_DIG"
#endif

#if FLT128X_MANT_DIG < 128 || FLT128X_MAX_EXP < 65536 || FLT128X_MIN_EXP + FLT128X_MAX_EXP != 3
# error "_Float128x bad format"
#endif
