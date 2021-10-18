/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS 8

 void NOINLINE
emulate_vcvtsh2ss(V512 * dest, V512 op1, V512 op2,
                __mmask8 k, int zero_mask)
{
    V512 v1, v2, v3, v4, v5, v6, v7, v8;
    int i;

    unpack_ph_2twops(op2, &v3, &v4);
    if ((k&1) || !k)
      v5.f32[0] = v3.f32[0];
    else if (zero_mask)
      v5.f32[0] = 0;
    else
      v5.f32[0] = dest->f32[0];

    for (i = 1; i < 4; i++)
      v5.f32[i] = op1.f32[i];

    *dest = v5;
}

void
test_512 (void)
{
  V512 res;
  V512 exp;

  init_src();
  emulate_vcvtsh2ss(&exp, src1, src2, 0x1, 0);
  res.xmm[0] = _mm_cvt_roundsh_ss(src1.xmm[0], src2.xmmh[0],
                                 _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "_mm_cvt_roundsh_ss");

  init_dest(&res, &exp);
  emulate_vcvtsh2ss(&exp, src1, src2, 0x1, 0);
  res.xmm[0] = _mm_mask_cvt_roundsh_ss(res.xmm[0], 0x1, src1.xmm[0],
                                      src2.xmmh[0], _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "mm_mask_cvt_roundsh_ss");

  emulate_vcvtsh2ss(&exp, src1, src2, 0x2, 1);
  res.xmm[0] = _mm_maskz_cvt_roundsh_ss(0x2, src1.xmm[0],
                                       src2.xmmh[0], _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "mm_maskz_cvt_roundsh_ss");

  if (n_errs != 0) {
      abort ();
  }
}


