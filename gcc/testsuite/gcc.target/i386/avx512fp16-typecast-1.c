/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */

#define AVX512FP16
#include "avx512fp16-helper.h"

void
test_512 (void)
{
  V512 res;

  res.ymmh[0] = _mm512_castph512_ph256 (src1.zmmh);
  check_results (&res, &src1, 16, "_mm512_castph512_ph256");

  res.xmmh[0] = _mm512_castph512_ph128 (src1.zmmh);
  check_results (&res, &src1, 8, "_mm512_castph512_ph128");

  res.zmmh = _mm512_castph256_ph512 (src1.ymmh[0]);
  check_results (&res, &src1, 16, "_mm512_castph256_ph512");

  res.zmmh = _mm512_castph128_ph512 (src1.xmmh[0]);
  check_results (&res, &src1, 8, "_mm512_castph128_ph512");

  res.zmm = _mm512_castph_ps (src1.zmmh);
  check_results (&res, &src1, 32, "_mm512_castph_ps");

  res.zmmd = _mm512_castph_pd (src1.zmmh);
  check_results (&res, &src1, 32, "_mm512_castph_pd");

  res.zmmi = _mm512_castph_si512 (src1.zmmh);
  check_results (&res, &src1, 32, "_mm512_castph_si512");

  res.zmmh = _mm512_castps_ph (src1.zmm);
  check_results (&res, &src1, 32, "_mm512_castps_ph");

  res.zmmh = _mm512_castpd_ph (src1.zmmd);
  check_results (&res, &src1, 32, "_mm512_castpd_ph");

  res.zmmh = _mm512_castsi512_ph (src1.zmmi);
  check_results (&res, &src1, 32, "_mm512_castsi512_ph");

  if (n_errs != 0)
    abort ();
}
