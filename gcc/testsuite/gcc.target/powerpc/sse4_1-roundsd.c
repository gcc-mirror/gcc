/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include <stdio.h>
#define NO_WARN_X86_INTRINSICS 1
#include <smmintrin.h>

#define VEC_T __m128d
#define FP_T double

#define ROUND_INTRIN(x, y, mode) _mm_round_sd (x, y, mode)

#include "sse4_1-round-data.h"

static struct data2 data[] = {
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0.00, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0.25, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0.50, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0.75, IGNORED } } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.ffffffffffffcp+50, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.ffffffffffffdp+50, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.ffffffffffffep+50, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffffffffffp+50, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000000p+51, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000001p+51, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000002p+51, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000003p+51, IGNORED } } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.ffffffffffffep+51, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.fffffffffffffp+51, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000000p+52, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = {  0x1.0000000000001p+52, IGNORED } } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000001p+52, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000000p+52, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.fffffffffffffp+51, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.ffffffffffffep+51, IGNORED } } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000004p+51, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000002p+51, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000001p+51, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.0000000000000p+51, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.ffffffffffffcp+50, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.ffffffffffffep+50, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.ffffffffffffdp+50, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0x1.ffffffffffffcp+50, IGNORED } } },

  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -1.00, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0.75, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0.50, IGNORED } } },
  { .value1 = { .f = { IGNORED, PASSTHROUGH } },
    .value2 = { .f = { -0.25, IGNORED } } }
};

static union value answers_NEAREST_INT[] = {
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  1.00, PASSTHROUGH } },

  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000002p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000004p+51, PASSTHROUGH } },

  { .f = {  0x1.ffffffffffffep+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = {  0x1.0000000000001p+52, PASSTHROUGH } },

  { .f = { -0x1.0000000000001p+52, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffep+51, PASSTHROUGH } },

  { .f = { -0x1.0000000000004p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000002p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },

  { .f = { -1.00, PASSTHROUGH } },
  { .f = { -1.00, PASSTHROUGH } },
  { .f = { -0.00, PASSTHROUGH } },
  { .f = { -0.00, PASSTHROUGH } }
};

static union value answers_NEG_INF[] = {
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },

  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000002p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000002p+51, PASSTHROUGH } },

  { .f = {  0x1.ffffffffffffep+51, PASSTHROUGH } },
  { .f = {  0x1.ffffffffffffep+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = {  0x1.0000000000001p+52, PASSTHROUGH } },

  { .f = { -0x1.0000000000001p+52, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffep+51, PASSTHROUGH } },

  { .f = { -0x1.0000000000004p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000002p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000002p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },

  { .f = { -1.00, PASSTHROUGH } },
  { .f = { -1.00, PASSTHROUGH } },
  { .f = { -1.00, PASSTHROUGH } },
  { .f = { -1.00, PASSTHROUGH } }
};

static union value answers_POS_INF[] = {
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  1.00, PASSTHROUGH } },
  { .f = {  1.00, PASSTHROUGH } },
  { .f = {  1.00, PASSTHROUGH } },

  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000002p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000002p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000004p+51, PASSTHROUGH } },

  { .f = {  0x1.ffffffffffffep+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = {  0x1.0000000000001p+52, PASSTHROUGH } },

  { .f = { -0x1.0000000000001p+52, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffep+51, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffep+51, PASSTHROUGH } },

  { .f = { -0x1.0000000000004p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000002p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },

  { .f = { -1.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } }
};

static union value answers_ZERO[] = {
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },

  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000002p+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000002p+51, PASSTHROUGH } },

  { .f = {  0x1.ffffffffffffep+51, PASSTHROUGH } },
  { .f = {  0x1.ffffffffffffep+51, PASSTHROUGH } },
  { .f = {  0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = {  0x1.0000000000001p+52, PASSTHROUGH } },

  { .f = { -0x1.0000000000001p+52, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+52, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffep+51, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffep+51, PASSTHROUGH } },

  { .f = { -0x1.0000000000004p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000002p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = { -0x1.0000000000000p+51, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },
  { .f = { -0x1.ffffffffffffcp+50, PASSTHROUGH } },

  { .f = { -1.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } },
  { .f = {  0.00, PASSTHROUGH } }
};

union value *answers[] = {
  answers_NEAREST_INT,
  answers_NEG_INF,
  answers_POS_INF,
  answers_ZERO,
  0 /* CUR_DIRECTION answers depend on current rounding mode.  */
};

#include "sse4_1-round3.h"
