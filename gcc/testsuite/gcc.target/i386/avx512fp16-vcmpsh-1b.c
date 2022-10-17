/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS 8

__mmask8 NOINLINE
emulate_cmp_sh(V512 op1, V512 op2,
	       __mmask8 k, int predicate)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  __mmask8 mr = 0;

  unpack_ph_2twops(op1, &v1, &v2);
  unpack_ph_2twops(op2, &v3, &v4);

  if ((k&1) || !k)
    mr = v1.f32[0] == v3.f32[0] ? 1 : 0;

  return mr;
}

void
test_512 (void)
{
  __mmask8 res, exp;

  init_src();

  exp = emulate_cmp_sh(src1, src2,  0x1, 0);
  res = _mm_cmp_round_sh_mask(src1.xmmh[0], src2.xmmh[0], 0, 8);
  check_results_mask(res, exp, 1, "_mm_cmp_round_sh_mask");

  exp = emulate_cmp_sh(src1, src2,  0x1, 0);
  res = _mm_mask_cmp_round_sh_mask(0x1, src1.xmmh[0], src2.xmmh[0], 0, 8);
  check_results_mask(res, exp, 1, "_mm_mask_cmp_round_sh_mask");

  if (n_errs != 0) {
      abort ();
  }
}

