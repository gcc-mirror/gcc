/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-O2 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#define NO_WARN_X86_INTRINSICS 1
#include <smmintrin.h>

#define VEC_T __m128
#define FP_T float

#define ROUND_INTRIN(x, mode) _mm_ceil_ps (x)

#include "sse4_1-round-data.h"

static struct data data[] = {
  { { .f = {  0.00,  0.25,  0.50,  0.75 } }, {  0.0,  1.0,  1.0,  1.0 } },

  { { .f = {  0x1.fffff8p+21,  0x1.fffffap+21,
	      0x1.fffffcp+21,  0x1.fffffep+21 } },
           {  0x1.fffff8p+21,  0x1.000000p+22,
	      0x1.000000p+22,  0x1.000000p+22 } },

  { { .f = {  0x1.fffffap+22,  0x1.fffffcp+22,
	      0x1.fffffep+22,  0x1.fffffep+23 } },
           {  0x1.fffffcp+22,  0x1.fffffcp+22,
	      0x1.000000p+23,  0x1.fffffep+23 } },

  { { .f = { -0x1.fffffep+23, -0x1.fffffep+22,
	     -0x1.fffffcp+22, -0x1.fffffap+22 } },
           { -0x1.fffffep+23, -0x1.fffffcp+22,
	     -0x1.fffffcp+22, -0x1.fffff8p+22 } },

  { { .f = { -0x1.fffffep+21, -0x1.fffffcp+21,
	     -0x1.fffffap+21, -0x1.fffff8p+21 } },
           { -0x1.fffff8p+21, -0x1.fffff8p+21,
	     -0x1.fffff8p+21, -0x1.fffff8p+21 } },

  { { .f = { -1.00, -0.75, -0.50, -0.25 } }, { -1.0,  0.0,  0.0,  0.0 } }
};

#include "sse4_1-round.h"
