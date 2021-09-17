/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS 8

void NOINLINE
emulate_vcvtsd2sh(V512 * dest, V512 op1, V512 op2,
                __mmask8 k, int zero_mask)
{
    V512 v1, v2, v5, v6, v7, v8;
    int i;
    
    unpack_ph_2twops(op1, &v1, &v2);
    unpack_ph_2twops(*dest, &v7, &v8);

    if ((k&1) || !k)
      v5.f32[0] = (float)op2.f64[0];
    else if (zero_mask)
      v5.f32[0] = 0;
    else
      v5.f32[0] = v7.f32[0];

    for (i = 1; i < 8; i++)
      v5.f32[i] = v1.f32[i];

    *dest = pack_twops_2ph(v5, v6);
}

void
test_512 (void)
{
  V512 res;
  V512 exp;

  init_src();
  emulate_vcvtsd2sh(&exp, src1, src2, 0x1, 0);
  res.xmmh[0] = _mm_cvt_roundsd_sh(src1.xmmh[0], src2.xmmd[0],
                                 _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "_mm_cvt_roundsd_sh");

  init_dest(&res, &exp);
  emulate_vcvtsd2sh(&exp, src1, src2, 0x1, 0);
  res.xmmh[0] = _mm_mask_cvt_roundsd_sh(res.xmmh[0], 0x1, src1.xmmh[0],
                                      src2.xmmd[0], _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "mm_mask_cvt_roundsd_sh");

  emulate_vcvtsd2sh(&exp, src1, src2, 0x2, 1);
  res.xmmh[0] = _mm_maskz_cvt_roundsd_sh(0x2, src1.xmmh[0],
                                       src2.xmmd[0], _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "mm_maskz_cvt_roundsd_sh");

  if (n_errs != 0) {
      abort ();
  }
}

