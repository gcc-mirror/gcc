/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS 8

void NOINLINE
EMULATE(c_fmul_csh) (V512 * dest, V512 op1, V512 op2,
		    __mmask8 k, int zero_mask, int c_flag)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;
  int invert = 1;
  if (c_flag == 1)
    invert = -1;

  unpack_ph_2twops(op1, &v1, &v2);
  unpack_ph_2twops(op2, &v3, &v4);
  unpack_ph_2twops(*dest, &v7, &v8);

  if ((k&1) || !k) {
    v5.f32[0] = v1.f32[0] * v3.f32[0]
      - invert * (v1.f32[1] * v3.f32[1]);
    v5.f32[1] = v1.f32[1] * v3.f32[0]
      + invert * (v1.f32[0] * v3.f32[1]);
  }
  else if (zero_mask)
    v5.f32[0] = 0;
  else
    v5.f32[0] = v7.f32[0];

  for (i = 2; i < 8; i++)
    v5.f32[i] = v1.f32[i];

  *dest = pack_twops_2ph(v5, v6);
}

void
TEST (void)
{
  V512 res;
  V512 exp;

  init_src();

  init_dest(&res, &exp);
  EMULATE(c_fmul_csh)(&exp, src1, src2,  0x1, 0 , 1);
  res.xmmh[0] = _mm_fcmul_round_sch(src1.xmmh[0], src2.xmmh[0], _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mm_fcmul_sch);

  init_dest(&res, &exp);
  EMULATE(c_fmul_csh)(&exp, src1, src2,  0x1, 0, 1);
  res.xmmh[0] = _mm_mask_fcmul_round_sch(res.xmmh[0], 0x1,
					 src1.xmmh[0], src2.xmmh[0],
					 _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mm_mask_fcmul_sch);

  init_dest(&res, &exp);
  EMULATE(c_fmul_csh)(&exp, src1, src2,  0x3, 1, 1);
  res.xmmh[0] = _mm_maskz_fcmul_round_sch(0x3, src1.xmmh[0],
					  src2.xmmh[0], _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mm_maskz_fcmul_sch);

  if (n_errs != 0) {
      abort ();
  }
}

