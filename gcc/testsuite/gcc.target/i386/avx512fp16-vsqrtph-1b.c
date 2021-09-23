/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 16)

void NOINLINE
EMULATE(sqrt_ph) (V512 * dest, V512 op1,
		__mmask32 k, int zero_mask)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;
  __mmask16 m1, m2;

  m1 = k & 0xffff;
  m2 = (k >> 16) & 0xffff;

  unpack_ph_2twops(op1, &v1, &v2);
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
      v5.f32[i] = sqrtf(v1.f32[i]);
    }

    if (((1 << i) & m2) == 0) {
      if (zero_mask) {
	v6.f32[i] = 0;
      }
      else {
	v6.u32[i] = v8.u32[i];
      }
    }
    else {
      v6.f32[i] = sqrtf(v2.f32[i]);
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

  EMULATE(sqrt_ph) (&exp, src1, NET_MASK, 0);
  HF(res) = INTRINSIC (_sqrt_ph) (HF(src1));
  CHECK_RESULT (&res, &exp, N_ELEMS, _sqrt_ph);

  init_dest(&res, &exp);
  EMULATE(sqrt_ph) (&exp, src1, MASK_VALUE, 0);
  HF(res) = INTRINSIC (_mask_sqrt_ph) (HF(res), MASK_VALUE, HF(src1));
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_sqrt_ph);

  EMULATE(sqrt_ph) (&exp, src1, ZMASK_VALUE, 1);
  HF(res) = INTRINSIC (_maskz_sqrt_ph) (ZMASK_VALUE, HF(src1));
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_sqrt_ph);

#if AVX512F_LEN == 512
  EMULATE(sqrt_ph) (&exp, src1, NET_MASK, 0);
  HF(res) = INTRINSIC (_sqrt_round_ph) (HF(src1), 8);
  CHECK_RESULT (&res, &exp, N_ELEMS, _sqrt_round_ph);

  init_dest(&res, &exp);
  EMULATE(sqrt_ph) (&exp, src1, MASK_VALUE, 0);
  HF(res) = INTRINSIC (_mask_sqrt_round_ph) (HF(res), MASK_VALUE, HF(src1), 8);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_sqrt_round_ph);

  EMULATE(sqrt_ph) (&exp, src1,  ZMASK_VALUE, 1);
  HF(res) = INTRINSIC (_maskz_sqrt_round_ph) (ZMASK_VALUE, HF(src1), 8);
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_sqrt_round_ph);
#endif

  if (n_errs != 0)
    abort ();
}

