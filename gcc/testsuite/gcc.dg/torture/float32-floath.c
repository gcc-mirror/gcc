/* Test _Float32 <float.h> macros.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float32_runtime } */

#define WIDTH 32
#define EXT 0
#include "floatn-floath.h"

#ifndef FLT32_MANT_DIG
# error "FLT32_MANT_DIG undefined"
#endif

#ifndef FLT32_DECIMAL_DIG
# error "FLT32_DECIMAL_DIG undefined"
#endif

#ifndef FLT32_DIG
# error "FLT32_DIG undefined"
#endif

#ifndef FLT32_MIN_EXP
# error "FLT32_MIN_EXP undefined"
#endif

#ifndef FLT32_MIN_10_EXP
# error "FLT32_MIN_10_EXP undefined"
#endif

#ifndef FLT32_MAX_EXP
# error "FLT32_MAX_EXP undefined"
#endif

#ifndef FLT32_MAX_10_EXP
# error "FLT32_MAX_10_EXP undefined"
#endif

#ifndef FLT32_MAX
# error "FLT32_MAX undefined"
#endif

#ifndef FLT32_EPSILON
# error "FLT32_EPSILON undefined"
#endif

#ifndef FLT32_MIN
# error "FLT32_MIN undefined"
#endif

#ifndef FLT32_TRUE_MIN
# error "FLT32_TRUE_MIN undefined"
#endif

#if FLT32_MANT_DIG != 24 || FLT32_MAX_EXP != 128 || FLT32_MIN_EXP != -125
# error "_Float32 bad format"
#endif
