/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-skip-if "no M_PI" { vxworks_kernel } } */

#include "sse4_1-check.h"

#define VEC_T __m128
#define FP_T float

#define ROUND_INTRIN(x, mode) _mm_ceil_ss(x, x)
#define ROUND_MODE _MM_FROUND_CEIL
#define CHECK_ROUND_MODE 0x02

#define LOOP_INCREMENT 4
#define CHECK_LOOP_INCREMENT 4

#include "sse4_1-round.h"
