/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */
/* { dg-skip-if "no M_PI" { vxworks_kernel } } */

#include "sse4_1-check.h"

#define VEC_T __m128
#define FP_T float

#define ROUND_INTRIN _mm_round_ps
#define ROUND_MODE _MM_FROUND_NINT
#define CHECK_ROUND_MODE 0x00

#define LOOP_INCREMENT 4
#define CHECK_LOOP_INCREMENT 1

#include "sse4_1-round.h"
