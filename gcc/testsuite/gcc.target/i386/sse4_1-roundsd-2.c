/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#define VEC_T __m128d
#define FP_T double
#define ASM_SUFFIX "l"

#define ROUND_INTRIN(x, mode) _mm_round_sd(x, x, mode)
#define ROUND_MODE _MM_FROUND_NINT
#define CHECK_ROUND_MODE 0x00

#define LOOP_INCREMENT 2
#define CHECK_LOOP_INCREMENT 2

#include "sse4_1-round.h"
