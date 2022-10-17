/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 16)

#ifndef __REDUCEPH__
#define __REDUCEPH__
V512 borrow_reduce_ps(V512 v, int imm8)
{
  V512 temp;
  switch (imm8)
    {
    case 1: temp.zmm =  _mm512_mask_reduce_ps (v.zmm, 0xffff, v.zmm, 1);break;
    case 2: temp.zmm =  _mm512_mask_reduce_ps (v.zmm, 0xffff, v.zmm, 2);break;
    case 3: temp.zmm =  _mm512_mask_reduce_ps (v.zmm, 0xffff, v.zmm, 3);break;
    case 4: temp.zmm =  _mm512_mask_reduce_ps (v.zmm, 0xffff, v.zmm, 4);break;
    case 5: temp.zmm =  _mm512_mask_reduce_ps (v.zmm, 0xffff, v.zmm, 5);break;
    case 6: temp.zmm =  _mm512_mask_reduce_ps (v.zmm, 0xffff, v.zmm, 6);break;
    case 7: temp.zmm =  _mm512_mask_reduce_ps (v.zmm, 0xffff, v.zmm, 7);break;
    case 8: temp.zmm =  _mm512_mask_reduce_ps (v.zmm, 0xffff, v.zmm, 8);break;
    }
  return temp;
}
#endif

void NOINLINE
EMULATE(reduce_ph) (V512 * dest, V512 op1,
		  __mmask32 k, int imm8, int zero_mask)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  V512 t1,t2;
  int i;
  __mmask16 m1, m2;

  m1 = k & 0xffff;
  m2 = (k >> 16) & 0xffff;

  unpack_ph_2twops(op1, &v1, &v2);
  unpack_ph_2twops(*dest, &v7, &v8);
  t1 = borrow_reduce_ps(v1, imm8);
  t2 = borrow_reduce_ps(v2, imm8);

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
	  v5.f32[i] = t1.f32[i];
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
	  v6.f32[i] = t2.f32[i];
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

  EMULATE(reduce_ph) (&exp, src1,  NET_MASK, 6, 0);
  HF(res) = INTRINSIC (_reduce_ph) (HF(src1), 6);
  CHECK_RESULT (&res, &exp, N_ELEMS, _reduce_ph);

  init_dest(&res, &exp);
  EMULATE(reduce_ph) (&exp, src1, MASK_VALUE, 5, 0);
  HF(res) = INTRINSIC (_mask_reduce_ph) (HF(res), MASK_VALUE, HF(src1), 5);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_reduce_ph);

  EMULATE(reduce_ph) (&exp, src1,  ZMASK_VALUE, 4, 1);
  HF(res) = INTRINSIC (_maskz_reduce_ph) (ZMASK_VALUE, HF(src1), 4);
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_reduce_ph);

#if AVX512F_LEN == 512
  EMULATE(reduce_ph) (&exp, src1,  NET_MASK, 6, 0);
  HF(res) = INTRINSIC (_reduce_round_ph) (HF(src1), 6, _ROUND_CUR);
  CHECK_RESULT (&res, &exp, N_ELEMS, _reduce_round_ph);

  init_dest(&res, &exp);
  EMULATE(reduce_ph) (&exp, src1, MASK_VALUE, 5, 0);
  HF(res) = INTRINSIC (_mask_reduce_round_ph) (HF(res), MASK_VALUE, HF(src1), 5, _ROUND_CUR);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_reduce_round_ph);

  EMULATE(reduce_ph) (&exp, src1,  ZMASK_VALUE, 4, 1);
  HF(res) = INTRINSIC (_maskz_reduce_round_ph) (ZMASK_VALUE, HF(src1), 4, _ROUND_CUR);
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_reduce_round_ph);
#endif

  if (n_errs != 0) {
      abort ();
  }
}

