/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 16)

void NOINLINE
EMULATE(cvtph2_d) (V512 * dest, V512 op1,
		 __mmask32 k, int zero_mask)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;
  __mmask16 m1, m2;

  m1 = k & 0xffff;

  unpack_ph_2twops(op1, &v1, &v2);

  for (i = 0; i < 16; i++) {
    if (((1 << i) & m1) == 0) {
      if (zero_mask) {
	v5.u32[i] = 0;
      }
      else {
	v5.u32[i] = dest->u32[i];
      }
    }
    else {
      v5.u32[i] = v1.f32[i];

    }
  }
  *dest = v5;
}

void
TEST (void)
{
  V512 res;
  V512 exp;

  init_src();

  EMULATE(cvtph2_d)(&exp, src1,  NET_MASK, 0);
  SI(res) = INTRINSIC (_cvttph_epu32) (H_HF(src1));
  CHECK_RESULT (&res, &exp, N_ELEMS, _cvttph_epu32);

  init_dest(&res, &exp);
  EMULATE(cvtph2_d)(&exp, src1, HALF_MASK, 0);
  SI(res) = INTRINSIC (_mask_cvttph_epu32) (SI(res), HALF_MASK, H_HF(src1));
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_cvttph_epu32);

  EMULATE(cvtph2_d)(&exp, src1,  HALF_MASK, 1);
  SI(res) = INTRINSIC (_maskz_cvttph_epu32) (HALF_MASK, H_HF(src1));
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_cvttph_epu32);

#if AVX512F_LEN == 512
  EMULATE(cvtph2_d)(&exp, src1,  NET_MASK, 0);
  SI(res) = INTRINSIC (_cvtt_roundph_epu32) (H_HF(src1), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _cvtt_roundph_epu32);

  init_dest(&res, &exp);
  EMULATE(cvtph2_d)(&exp, src1, HALF_MASK, 0);
  SI(res) = INTRINSIC (_mask_cvtt_roundph_epu32) (SI(res), HALF_MASK, H_HF(src1), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_cvtt_roundph_epu32);

  EMULATE(cvtph2_d)(&exp, src1,  HALF_MASK, 1);
  SI(res) = INTRINSIC (_maskz_cvtt_roundph_epu32) (HALF_MASK, H_HF(src1), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_cvtt_roundph_epu32);
#endif

  if (n_errs != 0) {
      abort ();
  }
}

