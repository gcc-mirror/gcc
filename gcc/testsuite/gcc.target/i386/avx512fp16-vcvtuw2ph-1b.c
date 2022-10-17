/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 16)

void NOINLINE
EMULATE(cvtw2_ph) (V512 * dest, V512 op1,
		 __mmask32 k, int zero_mask)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;
  __mmask16 m1, m2;

  m1 = k & 0xffff;
  m2 = (k >> 16) & 0xffff;

  unpack_ph_2twops(*dest, &v7, &v8);

  for (i = 0; i < 16; i++) {
      if (((1 << i) & m1) == 0) {
	  if (zero_mask) {
	      v5.f32[i] = 0;
	  }
	  else {
	      v5.f32[i] = v7.f32[i];
	  }
      }
      else {
	  v5.f32[i] = op1.u16[i];

      }

      if (((1 << i) & m2) == 0) {
	  if (zero_mask) {
	      v6.f32[i] = 0;
	  }
	  else {
	      v6.f32[i] = v8.f32[i];
	  }
      }
      else {
	  v6.f32[i] = op1.u16[i+16];
      }
  }

  *dest = pack_twops_2ph(v5, v6);
}

void
TEST (void)
{
  V512 res;
  V512 exp;

  init_src();

  EMULATE(cvtw2_ph)(&exp, src3, NET_MASK, 0);
  HF(res) = INTRINSIC (_cvtepu16_ph) (SI(src3));
  CHECK_RESULT (&res, &exp, N_ELEMS, _cvtepu16_ph);

  init_dest(&res, &exp);
  EMULATE(cvtw2_ph)(&exp, src3, MASK_VALUE, 0);
  HF(res) = INTRINSIC (_mask_cvtepu16_ph) (HF(res), MASK_VALUE, SI(src3));
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_cvtepu16_ph);

  EMULATE(cvtw2_ph)(&exp, src3, ZMASK_VALUE, 1);
  HF(res) = INTRINSIC (_maskz_cvtepu16_ph) (ZMASK_VALUE, SI(src3));
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_cvtepu16_ph);

#if AVX512F_LEN == 512
  EMULATE(cvtw2_ph)(&exp, src3, NET_MASK, 0);
  HF(res) = INTRINSIC (_cvt_roundepu16_ph) (SI(src3), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _cvt_roundepu16_ph);

  init_dest(&res, &exp);
  EMULATE(cvtw2_ph)(&exp, src3, MASK_VALUE, 0);
  HF(res) = INTRINSIC (_mask_cvt_roundepu16_ph) (HF(res), MASK_VALUE, SI(src3), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_cvt_roundepu16_ph);

  EMULATE(cvtw2_ph)(&exp, src3, ZMASK_VALUE, 1);
  HF(res) = INTRINSIC (_maskz_cvt_roundepu16_ph) (ZMASK_VALUE, SI(src3), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_cvt_roundepu16_ph);
#endif

  if (n_errs != 0) {
      abort ();
  }
}

