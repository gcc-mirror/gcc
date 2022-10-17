/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include <stdio.h>
#define NO_WARN_X86_INTRINSICS 1
#include <smmintrin.h>

#define VEC_T __m128
#define FP_T float

#define ROUND_INTRIN(x, y, mode) _mm_round_ss (x, y, mode)

#include "sse4_1-round-data.h"

static struct data2 data[] = {
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0.00, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0.25, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0.50, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0.75, IGNORED, IGNORED, IGNORED } } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffff8p+21, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffap+21, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffcp+21, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffep+21, IGNORED, IGNORED, IGNORED } } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffap+22, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffcp+22, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffep+22, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffep+23, IGNORED, IGNORED, IGNORED } } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffep+23, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffep+22, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffcp+22, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffap+22, IGNORED, IGNORED, IGNORED } } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffep+21, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffcp+21, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffap+21, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffff8p+21, IGNORED, IGNORED, IGNORED } } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -1.00, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0.75, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0.50, IGNORED, IGNORED, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
    .value2 = { .f = { -0.25, IGNORED, IGNORED, IGNORED } } }
};

static union value answers_NEAREST_INT[] = {
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = {  0x1.fffff8p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.000000p+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffffep+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -0x1.fffffep+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.000000p+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } }
};

static union value answers_NEG_INF[] = {
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = {  0x1.fffff8p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffffep+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -0x1.fffffep+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.000000p+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } }
};

static union value answers_POS_INF[] = {
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.000000p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = {  0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.000000p+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffffep+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -0x1.fffffep+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } }
};

static union value answers_ZERO[] = {
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = {  0x1.fffff8p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0x1.fffffep+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -0x1.fffffep+23, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffffcp+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+22, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = { -0x1.fffff8p+21, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },

  { .f = { -1.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH, PASSTHROUGH, PASSTHROUGH } }
};

union value *answers[] = {
  answers_NEAREST_INT,
  answers_NEG_INF,
  answers_POS_INF,
  answers_ZERO,
  0 /* CUR_DIRECTION answers depend on current rounding mode.  */
};

#include "sse4_1-round3.h"
