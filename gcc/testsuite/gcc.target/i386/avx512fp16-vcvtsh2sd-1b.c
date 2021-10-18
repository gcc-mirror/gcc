/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS 8

void NOINLINE
emulate_vcvtsh2sd(V512 * dest, V512 op1, V512 op2,
                __mmask8 k, int zero_mask)
{
    V512 v1, v2, v3, v4, v5, v6, v7, v8;

    unpack_ph_2twops(op2, &v3, &v4);

    if ((k&1) || !k)
      v5.f64[0] = v3.f32[0];
    else if (zero_mask)
      v5.f64[0] = 0;
    else
      v5.f64[0] = dest->f64[0];

    v5.f64[1] = op1.f64[1];

    *dest = v5;
}

void
test_512 (void)
{
  V512 res;
  V512 exp;

  init_src();
  emulate_vcvtsh2sd(&exp, src1, src2, 0x1, 0);
  res.xmmd[0] = _mm_cvt_roundsh_sd(src1.xmmd[0], src2.xmmh[0],
                                 _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "_mm_cvt_roundsh_sd");

  init_dest(&res, &exp);
  emulate_vcvtsh2sd(&exp, src1, src2, 0x1, 0);
  res.xmmd[0] = _mm_mask_cvt_roundsh_sd(res.xmmd[0], 0x1, src1.xmmd[0],
                                      src2.xmmh[0], _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "mm_mask_cvt_roundsh_sd");

  emulate_vcvtsh2sd(&exp, src1, src2, 0x2, 1);
  res.xmmd[0] = _mm_maskz_cvt_roundsh_sd(0x2, src1.xmmd[0],
                                       src2.xmmh[0], _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "mm_maskz_cvt_roundsh_sd");

  if (n_errs != 0) {
      abort ();
  }
}

