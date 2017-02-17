/* Test _Float32x <float.h> macros.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32x } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float32x_runtime } */

#define WIDTH 32
#define EXT 1
#include "floatn-floath.h"

#ifndef FLT32X_MANT_DIG
# error "FLT32X_MANT_DIG undefined"
#endif

#ifndef FLT32X_DECIMAL_DIG
# error "FLT32X_DECIMAL_DIG undefined"
#endif

#ifndef FLT32X_DIG
# error "FLT32X_DIG undefined"
#endif

#ifndef FLT32X_MIN_EXP
# error "FLT32X_MIN_EXP undefined"
#endif

#ifndef FLT32X_MIN_10_EXP
# error "FLT32X_MIN_10_EXP undefined"
#endif

#ifndef FLT32X_MAX_EXP
# error "FLT32X_MAX_EXP undefined"
#endif

#ifndef FLT32X_MAX_10_EXP
# error "FLT32X_MAX_10_EXP undefined"
#endif

#ifndef FLT32X_MAX
# error "FLT32X_MAX undefined"
#endif

#ifndef FLT32X_EPSILON
# error "FLT32X_EPSILON undefined"
#endif

#ifndef FLT32X_MIN
# error "FLT32X_MIN undefined"
#endif

#ifndef FLT32X_TRUE_MIN
# error "FLT32X_TRUE_MIN undefined"
#endif

#if FLT32X_MANT_DIG < 32 || FLT32X_MAX_EXP < 1024 || FLT32X_MIN_EXP + FLT32X_MAX_EXP != 3
# error "_Float32x bad format"
#endif
