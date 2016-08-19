/* Test _Float64 <float.h> macros.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float64 } */
/* { dg-require-effective-target float64_runtime } */

#define WIDTH 64
#define EXT 0
#include "floatn-floath.h"

#ifndef FLT64_MANT_DIG
# error "FLT64_MANT_DIG undefined"
#endif

#ifndef FLT64_DECIMAL_DIG
# error "FLT64_DECIMAL_DIG undefined"
#endif

#ifndef FLT64_DIG
# error "FLT64_DIG undefined"
#endif

#ifndef FLT64_MIN_EXP
# error "FLT64_MIN_EXP undefined"
#endif

#ifndef FLT64_MIN_10_EXP
# error "FLT64_MIN_10_EXP undefined"
#endif

#ifndef FLT64_MAX_EXP
# error "FLT64_MAX_EXP undefined"
#endif

#ifndef FLT64_MAX_10_EXP
# error "FLT64_MAX_10_EXP undefined"
#endif

#ifndef FLT64_MAX
# error "FLT64_MAX undefined"
#endif

#ifndef FLT64_EPSILON
# error "FLT64_EPSILON undefined"
#endif

#ifndef FLT64_MIN
# error "FLT64_MIN undefined"
#endif

#ifndef FLT64_TRUE_MIN
# error "FLT64_TRUE_MIN undefined"
#endif

#if FLT64_DECIMAL_DIG > DECIMAL_DIG
# error "FLT64_DECIMAL_DIG > DECIMAL_DIG"
#endif

#if FLT64_MANT_DIG != 53 || FLT64_MAX_EXP != 1024 || FLT64_MIN_EXP != -1021
# error "_Float64 bad format"
#endif
