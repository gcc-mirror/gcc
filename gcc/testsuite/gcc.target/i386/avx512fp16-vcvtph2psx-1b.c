/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 32)
#define CHECK_ELEMS (AVX512F_LEN / 16)

void NOINLINE
EMULATE(cvtxph2_ps) (V512 * dest, V512 op1, int n_el,
		   __mmask32 k, int zero_mask)
{
    V512 v1, v2, v3, v4, v5, v6, v7, v8;
    int i;
    __mmask16 m1, m2;

    m1 = k & 0xffff;
    unpack_ph_2twops(op1, &v1, &v2);

    for (i = 0; i < n_el; i++) {
      if (((1 << i) & m1) == 0) {
	if (zero_mask) {
	  v5.u32[i] = 0;
	}
	else {
	  v5.u32[i] = dest->u32[i];
	}
      }
      else {
	v5.f32[i] = v1.f32[i];
      }
    }

    for (i = n_el; i < 16; i++)
      v5.u32[i] = 0;

    *dest = v5;
}

void
TEST (void)
{
  V512 res;
  V512 exp;

  init_src();

  EMULATE(cvtxph2_ps)(&exp, src1, N_ELEMS, 0xffff, 0);
  SF(res) = INTRINSIC (_cvtxph_ps) (H_HF(src1));
  CHECK_RESULT (&res, &exp, CHECK_ELEMS, _cvtxph_ps);
 
  init_dest(&res, &exp);
  EMULATE(cvtxph2_ps)(&exp, src1, N_ELEMS, 0xcc, 0);
  SF(res) = INTRINSIC (_mask_cvtxph_ps) (SF(res), 0xcc, H_HF(src1));
  CHECK_RESULT (&res, &exp, CHECK_ELEMS, _mask_cvtxph_ps);
 
  EMULATE(cvtxph2_ps)(&exp, src1, N_ELEMS, 0xc1, 1);
  SF(res) = INTRINSIC (_maskz_cvtxph_ps) (0xc1, H_HF(src1));
  CHECK_RESULT (&res, &exp, CHECK_ELEMS, _maskz_cvtxph_ps);

#if AVX512F_LEN == 512
  EMULATE(cvtxph2_ps)(&exp, src1, N_ELEMS, 0xffff, 0);
  SF(res) = INTRINSIC (_cvtx_roundph_ps) (H_HF(src1), _ROUND_CUR);
  CHECK_RESULT (&res, &exp, CHECK_ELEMS, _cvtx_roundph_ps);
 
  init_dest(&res, &exp);
  EMULATE(cvtxph2_ps)(&exp, src1, N_ELEMS, 0xcc, 0);
  SF(res) = INTRINSIC (_mask_cvtx_roundph_ps) (SF(res), 0xcc, H_HF(src1), _ROUND_CUR);
  CHECK_RESULT (&res, &exp, CHECK_ELEMS, _mask_cvtx_roundph_ps);
 
  EMULATE(cvtxph2_ps)(&exp, src1, N_ELEMS, 0xc1, 1);
  SF(res) = INTRINSIC (_maskz_cvtx_roundph_ps) (0xc1, H_HF(src1), _ROUND_CUR);
  CHECK_RESULT (&res, &exp, CHECK_ELEMS, _maskz_cvtx_roundph_ps);
#endif

  if (n_errs != 0) 
    abort ();
}

