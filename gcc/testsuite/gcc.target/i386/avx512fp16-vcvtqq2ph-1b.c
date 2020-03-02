/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 64)

void NOINLINE
EMULATE(cvtq2_ph) (V512 * dest, V512 op1, int n_el,
		 __mmask32 k, int zero_mask)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;
  __mmask16 m1, m2;

  m1 = k & 0xffff;

  unpack_ph_2twops(*dest, &v7, &v8);

  for (i = 0; i < n_el; i++) {
      if (((1 << i) & m1) == 0) {
	  if (zero_mask) {
	      v5.f32[i] = 0;
	  }
	  else {
	      v5.u32[i] = v7.u32[i];
	  }
      }
      else {
	  v5.f32[i] = op1.u64[i];
      }
  }

  // The left part should be zero
  for (i = n_el; i < 16; i++)
    v5.f32[i] = 0;

  *dest = pack_twops_2ph(v5, v5);
}

void
TEST (void)
{

  V512 res;
  V512 exp;

  init_src();

  EMULATE(cvtq2_ph)(&exp, src3, N_ELEMS, NET_MASK, 0);
  res.xmmh[0] = INTRINSIC (_cvtepi64_ph) (SI(src3));
  CHECK_RESULT (&res, &exp, 8, _cvtepi64_ph);

  init_dest(&res, &exp);
  EMULATE(cvtq2_ph)(&exp, src3, N_ELEMS, 0xcc, 0);
  res.xmmh[0] = INTRINSIC (_mask_cvtepi64_ph) (res.xmmh[0], 0xcc, SI(src3));
  CHECK_RESULT (&res, &exp, 8, _mask_cvtepi64_ph);

  EMULATE(cvtq2_ph)(&exp, src3, N_ELEMS, 0xf1, 1);
  res.xmmh[0] = INTRINSIC (_maskz_cvtepi64_ph) (0xf1, SI(src3));
  CHECK_RESULT (&res, &exp, 8, _maskz_cvtepi64_ph);

#if AVX512F_LEN == 512
  EMULATE(cvtq2_ph)(&exp, src3, N_ELEMS, NET_MASK, 0);
  res.xmmh[0] = INTRINSIC (_cvt_roundepi64_ph) (SI(src3), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, 8, _cvt_roundepi64_ph);

  init_dest(&res, &exp);
  EMULATE(cvtq2_ph)(&exp, src3, N_ELEMS, 0xcc, 0);
  res.xmmh[0] = INTRINSIC (_mask_cvt_roundepi64_ph) (res.xmmh[0], 0xcc, SI(src3), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, 8, _mask_cvt_roundepi64_ph);

  EMULATE(cvtq2_ph)(&exp, src3, N_ELEMS, 0xf1, 1);
  res.xmmh[0] = INTRINSIC (_maskz_cvt_roundepi64_ph) (0xf1, SI(src3), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, 8, _maskz_cvt_roundepi64_ph);
#endif

  if (n_errs != 0) {
      abort ();
  }
}

