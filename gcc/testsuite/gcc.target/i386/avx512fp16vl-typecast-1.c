/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mavx512dq" } */

#define AVX512FP16
#include "avx512fp16-helper.h"

void
test_512 (void)
{
  V512 res;
  res.xmm[0] = _mm_castph_ps (src1.xmmh[0]);
  check_results (&res, &src1, 8, "_mm_castph_ps");

  res.xmmd[0] = _mm_castph_pd (src1.xmmh[0]);
  check_results (&res, &src1, 8, "_mm_castph_pd");

  res.xmmi[0] = _mm_castph_si128 (src1.xmmh[0]);
  check_results (&res, &src1, 8, "_mm_castph_si128");

  res.xmmh[0] = _mm_castps_ph (src1.xmm[0]);
  check_results (&res, &src1, 8, "_mm_castps_ph");

  res.xmmh[0] = _mm_castpd_ph (src1.xmmd[0]);
  check_results (&res, &src1, 8, "_mm_castpd_ph");

  res.xmmh[0] = _mm_castsi128_ph (src1.xmmi[0]);
  check_results (&res, &src1, 8, "_mm_castsi128_ph");

  res.ymm[0] = _mm256_castph_ps (src1.ymmh[0]);
  check_results (&res, &src1, 16, "_mm256_castph_ps");

  res.ymmd[0] = _mm256_castph_pd (src1.ymmh[0]);
  check_results (&res, &src1, 16, "_mm256_castph_pd");

  res.ymmi[0] = _mm256_castph_si256 (src1.ymmh[0]);
  check_results (&res, &src1, 16, "_mm256_castph_si256");

  res.ymmh[0] = _mm256_castps_ph (src1.ymm[0]);
  check_results (&res, &src1, 16, "_mm256_castps_ph");

  res.ymmh[0] = _mm256_castpd_ph (src1.ymmd[0]);
  check_results (&res, &src1, 16, "_mm256_castpd_ph");

  res.ymmh[0] = _mm256_castsi256_ph (src1.ymmi[0]);
  check_results (&res, &src1, 16, "_mm256_castsi256_ph");

  res.xmmh[0] = _mm256_castph256_ph128 (src1.ymmh[0]);
  check_results (&res, &src1, 8, "_mm256_castph256_ph128");

  res.ymmh[0] = _mm256_castph128_ph256 (src1.xmmh[0]);
  check_results (&res, &src1, 8, "_mm256_castph128_ph256");
  
  if (n_errs != 0)
    abort ();
}
