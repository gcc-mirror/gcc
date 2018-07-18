/* Test _Float16 <float.h> macros.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float16 } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target float16_runtime } */

#define WIDTH 16
#define EXT 0
#include "floatn-floath.h"

#ifndef FLT16_MANT_DIG
# error "FLT16_MANT_DIG undefined"
#endif

#ifndef FLT16_DECIMAL_DIG
# error "FLT16_DECIMAL_DIG undefined"
#endif

#ifndef FLT16_DIG
# error "FLT16_DIG undefined"
#endif

#ifndef FLT16_MIN_EXP
# error "FLT16_MIN_EXP undefined"
#endif

#ifndef FLT16_MIN_10_EXP
# error "FLT16_MIN_10_EXP undefined"
#endif

#ifndef FLT16_MAX_EXP
# error "FLT16_MAX_EXP undefined"
#endif

#ifndef FLT16_MAX_10_EXP
# error "FLT16_MAX_10_EXP undefined"
#endif

#ifndef FLT16_MAX
# error "FLT16_MAX undefined"
#endif

#ifndef FLT16_EPSILON
# error "FLT16_EPSILON undefined"
#endif

#ifndef FLT16_MIN
# error "FLT16_MIN undefined"
#endif

#ifndef FLT16_TRUE_MIN
# error "FLT16_TRUE_MIN undefined"
#endif

#if FLT16_MANT_DIG != 11 || FLT16_MAX_EXP != 16 || FLT16_MIN_EXP != -13
# error "_Float16 bad format"
#endif
