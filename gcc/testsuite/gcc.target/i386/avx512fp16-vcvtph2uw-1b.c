/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 16)

void NOINLINE
EMULATE(cvtph2_w) (V512 * dest, V512 op1,
		 __mmask32 k, int zero_mask)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;
  __mmask16 m1, m2;

  m1 = k & 0xffff;
  m2 = (k >> 16) & 0xffff;

  unpack_ph_2twops(op1, &v1, &v2);

  for (i = 0; i < 16; i++) {
    if (((1 << i) & m1) == 0) {
      if (zero_mask) {
	dest->u16[i] = 0;
      }
    }
    else {
      dest->u16[i] = v1.f32[i];

    }

    if (((1 << i) & m2) == 0) {
      if (zero_mask) {
	dest->u16[i+16] = 0;
      }
    }
    else {
      dest->u16[i+16] = v2.f32[i];
    }
  }
}

void
TEST (void)
{
  V512 res;
  V512 exp;

  init_src();

  EMULATE(cvtph2_w)(&exp, src1,  NET_MASK, 0);
  SI(res) = INTRINSIC (_cvtph_epu16) (HF(src1));
  CHECK_RESULT (&res, &exp, N_ELEMS, _cvtph_epu16);

  init_dest(&res, &exp);
  EMULATE(cvtph2_w)(&exp, src1, MASK_VALUE, 0);
  SI(res) = INTRINSIC (_mask_cvtph_epu16) (SI(res), MASK_VALUE, HF(src1));
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_cvtph_epu16);

  EMULATE(cvtph2_w)(&exp, src1, ZMASK_VALUE, 1);
  SI(res) = INTRINSIC (_maskz_cvtph_epu16) (ZMASK_VALUE, HF(src1));
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_cvtph_epu16);

#if AVX512F_LEN == 512
  EMULATE(cvtph2_w)(&exp, src1,  NET_MASK, 0);
  SI(res) = INTRINSIC (_cvt_roundph_epu16) (HF(src1), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _cvt_roundph_epu16);

  init_dest(&res, &exp);
  EMULATE(cvtph2_w)(&exp, src1, MASK_VALUE, 0);
  SI(res) = INTRINSIC (_mask_cvt_roundph_epu16) (SI(res), MASK_VALUE, HF(src1), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_cvt_roundph_epu16);

  EMULATE(cvtph2_w)(&exp, src1, ZMASK_VALUE, 1);
  SI(res) = INTRINSIC (_maskz_cvt_roundph_epu16) (ZMASK_VALUE, HF(src1), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_cvt_roundph_epu16);
#endif

  if (n_errs != 0)
    abort ();
}

