/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define NO_WARN_X86_INTRINSICS 1
#include <smmintrin.h>

#define VEC_T __m128d
#define FP_T double

#define ROUND_INTRIN(x, ignored, mode) _mm_round_pd (x, mode)

#include "sse4_1-round-data.h"

struct data2 data[] = {
  { .value1 = { .f = {  0.00,  0.25 } } },
  { .value1 = { .f = {  0.50,  0.75 } } },

  { .value1 = { .f = {  0x1.ffffffffffffcp+50,  0x1.ffffffffffffdp+50 } } },
  { .value1 = { .f = {  0x1.ffffffffffffep+50,  0x1.fffffffffffffp+50 } } },
  { .value1 = { .f = {  0x1.0000000000000p+51,  0x1.0000000000001p+51 } } },
  { .value1 = { .f = {  0x1.0000000000002p+51,  0x1.0000000000003p+51 } } },

  { .value1 = { .f = {  0x1.ffffffffffffep+51,  0x1.fffffffffffffp+51 } } },
  { .value1 = { .f = {  0x1.0000000000000p+52,  0x1.0000000000001p+52 } } },

  { .value1 = { .f = { -0x1.0000000000001p+52, -0x1.0000000000000p+52 } } },
  { .value1 = { .f = { -0x1.fffffffffffffp+51, -0x1.ffffffffffffep+51 } } },

  { .value1 = { .f = { -0x1.0000000000004p+51, -0x1.0000000000002p+51 } } },
  { .value1 = { .f = { -0x1.0000000000001p+51, -0x1.0000000000000p+51 } } },
  { .value1 = { .f = { -0x1.ffffffffffffcp+50, -0x1.ffffffffffffep+50 } } },
  { .value1 = { .f = { -0x1.ffffffffffffdp+50, -0x1.ffffffffffffcp+50 } } },

  { .value1 = { .f = { -1.00, -0.75 } } },
  { .value1 = { .f = { -0.50, -0.25 } } }
};

union value answers_NEAREST_INT[] = {
  { .f = {  0.00,  0.00 } },
  { .f = {  0.00,  1.00 } },

  { .f = {  0x1.ffffffffffffcp+50,  0x1.ffffffffffffcp+50 } },
  { .f = {  0x1.0000000000000p+51,  0x1.0000000000000p+51 } },
  { .f = {  0x1.0000000000000p+51,  0x1.0000000000000p+51 } },
  { .f = {  0x1.0000000000002p+51,  0x1.0000000000004p+51 } },

  { .f = {  0x1.ffffffffffffep+51,  0x1.0000000000000p+52 } },
  { .f = {  0x1.0000000000000p+52,  0x1.0000000000001p+52 } },

  { .f = { -0x1.0000000000001p+52, -0x1.0000000000000p+52 } },
  { .f = { -0x1.0000000000000p+52, -0x1.ffffffffffffep+51 } },

  { .f = { -0x1.0000000000004p+51, -0x1.0000000000002p+51 } },
  { .f = { -0x1.0000000000000p+51, -0x1.0000000000000p+51 } },
  { .f = { -0x1.ffffffffffffcp+50, -0x1.0000000000000p+51 } },
  { .f = { -0x1.ffffffffffffcp+50, -0x1.ffffffffffffcp+50 } },

  { .f = { -1.00, -1.00 } },
  { .f = {  0.00,  0.00 } }
};

union value answers_NEG_INF[] = {
  { .f = {  0.00,  0.00 } },
  { .f = {  0.00,  0.00 } },

  { .f = {  0x1.ffffffffffffcp+50,  0x1.ffffffffffffcp+50 } },
  { .f = {  0x1.ffffffffffffcp+50,  0x1.ffffffffffffcp+50 } },
  { .f = {  0x1.0000000000000p+51,  0x1.0000000000000p+51 } },
  { .f = {  0x1.0000000000002p+51,  0x1.0000000000002p+51 } },

  { .f = {  0x1.ffffffffffffep+51,  0x1.ffffffffffffep+51 } },
  { .f = {  0x1.0000000000000p+52,  0x1.0000000000001p+52 } },

  { .f = { -0x1.0000000000001p+52, -0x1.0000000000000p+52 } },
  { .f = { -0x1.0000000000000p+52, -0x1.ffffffffffffep+51 } },

  { .f = { -0x1.0000000000004p+51, -0x1.0000000000002p+51 } },
  { .f = { -0x1.0000000000002p+51, -0x1.0000000000000p+51 } },
  { .f = { -0x1.ffffffffffffcp+50, -0x1.0000000000000p+51 } },
  { .f = { -0x1.0000000000000p+51, -0x1.ffffffffffffcp+50 } },

  { .f = { -1.00, -1.00 } },
  { .f = { -1.00, -1.00 } }
};

union value answers_POS_INF[] = {
  { .f = {  0.00,  1.00 } },
  { .f = {  1.00,  1.00 } },

  { .f = {  0x1.ffffffffffffcp+50,  0x1.0000000000000p+51 } },
  { .f = {  0x1.0000000000000p+51,  0x1.0000000000000p+51 } },
  { .f = {  0x1.0000000000000p+51,  0x1.0000000000002p+51 } },
  { .f = {  0x1.0000000000002p+51,  0x1.0000000000004p+51 } },

  { .f = {  0x1.ffffffffffffep+51,  0x1.0000000000000p+52 } },
  { .f = {  0x1.0000000000000p+52,  0x1.0000000000001p+52 } },

  { .f = { -0x1.0000000000001p+52, -0x1.0000000000000p+52 } },
  { .f = { -0x1.ffffffffffffep+51, -0x1.ffffffffffffep+51 } },

  { .f = { -0x1.0000000000004p+51, -0x1.0000000000002p+51 } },
  { .f = { -0x1.0000000000000p+51, -0x1.0000000000000p+51 } },
  { .f = { -0x1.ffffffffffffcp+50, -0x1.ffffffffffffcp+50 } },
  { .f = { -0x1.ffffffffffffcp+50, -0x1.ffffffffffffcp+50 } },

  { .f = { -1.00,  0.00 } },
  { .f = {  0.00,  0.00 } }
};

union value answers_ZERO[] = {
  { .f = {  0.00,  0.00 } },
  { .f = {  0.00,  0.00 } },

  { .f = {  0x1.ffffffffffffcp+50,  0x1.ffffffffffffcp+50 } },
  { .f = {  0x1.ffffffffffffcp+50,  0x1.ffffffffffffcp+50 } },
  { .f = {  0x1.0000000000000p+51,  0x1.0000000000000p+51 } },
  { .f = {  0x1.0000000000002p+51,  0x1.0000000000002p+51 } },

  { .f = {  0x1.ffffffffffffep+51,  0x1.ffffffffffffep+51 } },
  { .f = {  0x1.0000000000000p+52,  0x1.0000000000001p+52 } },

  { .f = { -0x1.0000000000001p+52, -0x1.0000000000000p+52 } },
  { .f = { -0x1.ffffffffffffep+51, -0x1.ffffffffffffep+51 } },

  { .f = { -0x1.0000000000004p+51, -0x1.0000000000002p+51 } },
  { .f = { -0x1.0000000000000p+51, -0x1.0000000000000p+51 } },
  { .f = { -0x1.ffffffffffffcp+50, -0x1.ffffffffffffcp+50 } },
  { .f = { -0x1.ffffffffffffcp+50, -0x1.ffffffffffffcp+50 } },

  { .f = { -1.00,  0.00 } },
  { .f = {  0.00,  0.00 } }
};

union value *answers[] = {
  answers_NEAREST_INT,
  answers_NEG_INF,
  answers_POS_INF,
  answers_ZERO,
  0 /* CUR_DIRECTION answers depend on current rounding mode.  */
};

#include "sse4_1-round3.h"
