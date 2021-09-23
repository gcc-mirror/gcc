/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 32)

void NOINLINE
EMULATE(cvtd2_ph) (V512 * dest, V512 op1,
		 __mmask32 k, int zero_mask)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;
  __mmask16 m1, m2;

  m1 = k & 0xffff;

  unpack_ph_2twops(*dest, &v7, &v8);

  for (i = 0; i < 16; i++) {
    if (((1 << i) & m1) == 0) {
      if (zero_mask) {
	v5.f32[i] = 0;
      }
      else {
	v5.u32[i] = v7.u32[i];
      }
    }
    else {
      v5.f32[i] = op1.u32[i];
    }
  }
  *dest = pack_twops_2ph(v5, v5);
}

void
TEST (void)
{
  V512 res;
  V512 exp;

  init_src();

  EMULATE(cvtd2_ph)(&exp, src3, NET_MASK, 0);
  H_HF(res) = INTRINSIC (_cvtepi32_ph) (SI(src3));
  CHECK_RESULT (&res, &exp, N_ELEMS, _cvtepi32_ph);

  init_dest(&res, &exp);
  EMULATE(cvtd2_ph)(&exp, src3, HALF_MASK, 0);
  H_HF(res) = INTRINSIC (_mask_cvtepi32_ph) (H_HF(res), HALF_MASK, SI(src3));
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_cvtepi32_ph);

  EMULATE(cvtd2_ph)(&exp, src3, HALF_MASK, 1);
  H_HF(res) = INTRINSIC (_maskz_cvtepi32_ph) (HALF_MASK, SI(src3));
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_cvtepi32_ph);

#if AVX512F_LEN == 512
  EMULATE(cvtd2_ph)(&exp, src3, NET_MASK, 0);
  H_HF(res) = INTRINSIC (_cvt_roundepi32_ph) (SI(src3), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _cvt_roundepi32_ph);

  init_dest(&res, &exp);
  EMULATE(cvtd2_ph)(&exp, src3, HALF_MASK, 0);
  H_HF(res) = INTRINSIC (_mask_cvt_roundepi32_ph) (H_HF(res), HALF_MASK, SI(src3), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_cvt_roundepi32_ph);

  EMULATE(cvtd2_ph)(&exp, src3, HALF_MASK, 1);
  H_HF(res) = INTRINSIC (_maskz_cvt_roundepi32_ph) (HALF_MASK, SI(src3), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_cvt_roundepi32_ph);
#endif

  if (n_errs != 0) {
      abort ();
  }
}


