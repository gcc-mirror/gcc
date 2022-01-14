/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define NO_WARN_X86_INTRINSICS 1
#include <smmintrin.h>

#define VEC_T __m128
#define FP_T float

#define ROUND_INTRIN(x, ignored, mode) _mm_round_ps (x, mode)

#include "sse4_1-round-data.h"

struct data2 data[] = {
  { .value1 = { .f = {  0.00,  0.25,  0.50,  0.75 } } },

  { .value1 = { .f = {  0x1.fffff8p+21,  0x1.fffffap+21,
			0x1.fffffcp+21,  0x1.fffffep+21 } } },
  { .value1 = { .f = {  0x1.fffffap+22,  0x1.fffffcp+22,
			0x1.fffffep+22,  0x1.fffffep+23 } } },
  { .value1 = { .f = { -0x1.fffffep+23, -0x1.fffffep+22,
		       -0x1.fffffcp+22, -0x1.fffffap+22 } } },
  { .value1 = { .f = { -0x1.fffffep+21, -0x1.fffffcp+21,
		       -0x1.fffffap+21, -0x1.fffff8p+21 } } },

  { .value1 = { .f = { -1.00, -0.75, -0.50, -0.25 } } }
};

union value answers_NEAREST_INT[] = {
  { .f = {  0.00,  0.00,  0.00,  1.00 } },

  { .f = {  0x1.fffff8p+21,  0x1.fffff8p+21,
            0x1.000000p+22,  0x1.000000p+22 } },
  { .f = {  0x1.fffff8p+22,  0x1.fffffcp+22,
            0x1.000000p+23,  0x1.fffffep+23 } },
  { .f = { -0x1.fffffep+23, -0x1.000000p+23,
           -0x1.fffffcp+22, -0x1.fffff8p+22 } },
  { .f = { -0x1.000000p+22, -0x1.000000p+22,
           -0x1.fffff8p+21, -0x1.fffff8p+21 } },

  { .f = { -1.00, -1.00,  0.00,  0.00 } }
};

union value answers_NEG_INF[] = {
  { .f = {  0.00,  0.00,  0.00,  0.00 } },

  { .f = {  0x1.fffff8p+21,  0x1.fffff8p+21,
            0x1.fffff8p+21,  0x1.fffff8p+21 } },
  { .f = {  0x1.fffff8p+22,  0x1.fffffcp+22,
            0x1.fffffcp+22,  0x1.fffffep+23 } },
  { .f = { -0x1.fffffep+23, -0x1.000000p+23,
           -0x1.fffffcp+22, -0x1.fffffcp+22 } },
  { .f = { -0x1.000000p+22, -0x1.000000p+22,
           -0x1.000000p+22, -0x1.fffff8p+21 } },

  { .f = { -1.00, -1.00, -1.00, -1.00 } }
};

union value answers_POS_INF[] = {
  { .f = {  0.00,  1.00,  1.00,  1.00 } },

  { .f = {  0x1.fffff8p+21,  0x1.000000p+22,
            0x1.000000p+22,  0x1.000000p+22 } },
  { .f = {  0x1.fffffcp+22,  0x1.fffffcp+22,
            0x1.000000p+23,  0x1.fffffep+23 } },
  { .f = { -0x1.fffffep+23, -0x1.fffffcp+22,
           -0x1.fffffcp+22, -0x1.fffff8p+22 } },
  { .f = { -0x1.fffff8p+21, -0x1.fffff8p+21,
           -0x1.fffff8p+21, -0x1.fffff8p+21 } },

  { .f = { -1.00,  0.00,  0.00,  0.00 } }
};

union value answers_ZERO[] = {
  { .f = {  0.00,  0.00,  0.00,  0.00 } },

  { .f = {  0x1.fffff8p+21,  0x1.fffff8p+21,
            0x1.fffff8p+21,  0x1.fffff8p+21 } },
  { .f = {  0x1.fffff8p+22,  0x1.fffffcp+22,
            0x1.fffffcp+22,  0x1.fffffep+23 } },
  { .f = { -0x1.fffffep+23, -0x1.fffffcp+22,
           -0x1.fffffcp+22, -0x1.fffff8p+22 } },
  { .f = { -0x1.fffff8p+21, -0x1.fffff8p+21,
           -0x1.fffff8p+21, -0x1.fffff8p+21 } },

  { .f = { -1.00,  0.00,  0.00,  0.00 } }
};

union value *answers[] = {
  answers_NEAREST_INT,
  answers_NEG_INF,
  answers_POS_INF,
  answers_ZERO,
  0 /* CUR_DIRECTION answers depend on current rounding mode.  */
};

#include "sse4_1-round3.h"
