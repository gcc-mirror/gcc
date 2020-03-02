/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS 8

void NOINLINE
EMULATE(c_fmadd_csh) (V512 * dest, V512 op1, V512 op2,
		    __mmask8 k, int zero_mask, int c_flag,
		    int is_mask3)
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
    v5.f32[0] = v1.f32[0] * v7.f32[0]
      - invert * (v1.f32[1] * v7.f32[1]) + v3.f32[0];
    v5.f32[1] = v1.f32[0] * v7.f32[1]
      + invert * (v1.f32[1] * v7.f32[0]) + v3.f32[1];
  }
  else if (zero_mask)
    v5.f32[0] = 0;
  else
    v5.f32[0] = v7.f32[0];

  for (i = 2; i < 8; i++)
    v5.f32[i] = is_mask3? v3.f32[i] : v7.f32[i];

  *dest = pack_twops_2ph(v5, v6);
}

void
TEST (void)
{
  V512 res;
  V512 exp;

  init_src();

  init_dest(&res, &exp);
  EMULATE(c_fmadd_csh)(&exp, src1, src2,  0x1, 0, 1, 0);
  res.xmmh[0] = _mm_fcmadd_round_sch(res.xmmh[0], src1.xmmh[0],
				     src2.xmmh[0], _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mm_fcmadd_sch);

  init_dest(&res, &exp);
  EMULATE(c_fmadd_csh)(&exp, src1, src2,  0x1, 0, 1, 0);
  res.xmmh[0] = _mm_mask_fcmadd_round_sch(res.xmmh[0], 0x1,
					  src1.xmmh[0], src2.xmmh[0], _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mm_mask_fcmadd_sch);

  init_dest(&res, &exp);
  EMULATE(c_fmadd_csh)(&exp, src1, src2,  0x1, 0, 1, 1);
  res.xmmh[0] = _mm_mask3_fcmadd_round_sch(res.xmmh[0], src1.xmmh[0], src2.xmmh[0],
					   0x1, _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mm_mask3_fcmadd_sch);

  init_dest(&res, &exp);
  EMULATE(c_fmadd_csh)(&exp, src1, src2,  0x3, 1, 1, 0);
  res.xmmh[0] = _mm_maskz_fcmadd_round_sch(0x3, res.xmmh[0], src1.xmmh[0],
					   src2.xmmh[0], _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mm_maskz_fcmadd_sch);

  if (n_errs != 0) {
      abort ();
  }
}

