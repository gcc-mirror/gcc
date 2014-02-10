/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#define SIZE (128 / 32)

#include <math.h>
#include "avx512f-check.h"
#include "avx512f-helper.h"

static void
compute_rndscaless (float *s1, float *s2, float *r, int imm)
{
  int rc, m;
  rc = imm & 0xf;
  m = imm >> 4;

  switch (rc)
    {
    case _MM_FROUND_FLOOR:
      r[0] = __builtin_floorf (s2[0] * pow (2, m)) / pow (2, m);
      break;
    case _MM_FROUND_CEIL:
      r[0] = __builtin_ceilf (s2[0] * pow (2, m)) / pow (2, m);
      break;
    default:
      abort ();
      break;
    }

  r[1] = s1[1];
  r[2] = s1[2];
  r[3] = s1[3];
}

static void
avx512f_test (void)
{
  int imm = _MM_FROUND_FLOOR | (7 << 4);
  union128 s1, s2, res1;
  float res_ref[SIZE];

  s1.x = _mm_set_ps (4.05084, -1.23162, 2.00231, -6.22103);
  s2.x = _mm_set_ps (-4.19319, -3.53222, 7.33527, 5.57655);

  res1.x = _mm_roundscale_ss (s1.x, s2.x, imm);

  compute_rndscaless (s1.a, s2.a, res_ref, imm);

  if (check_union128 (res1, res_ref))
    abort ();
}
