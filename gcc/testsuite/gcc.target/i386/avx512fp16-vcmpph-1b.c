/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */

#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 16)

__mmask32 NOINLINE
EMULATE(cmp_ph) (V512 op1, V512 op2,
	       __mmask32 k, int predicate)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i, j;
  __mmask16 mr1 = 0, mr2 = 0;
  __mmask16 m1, m2;
  __mmask32 mr = 0;

  m1 = k & 0xffff;
  m2 = (k >> 16) & 0xffff;

  unpack_ph_2twops(op1, &v1, &v2);
  unpack_ph_2twops(op2, &v3, &v4);

  for (i = 0; i < 16; i++) {
      if (((1 << i) & m1) != 0) {
	  j = v1.f32[i] == v3.f32[i] ? 1 : 0;
	  mr1 = mr1 | (j << i);
      }

      if (((1 << i) & m2) != 0) {
	  j = v2.f32[i] == v4.f32[i] ? 1 : 0;
	  mr2 = mr2 | (j << i);
      }
  }

  mr = mr1 | (mr2 << 16);
  return mr;
}

void
TEST (void)
{
  __mmask32 res, exp;

  init_src();

  exp = EMULATE(cmp_ph) (src1, src2,  NET_MASK, 0);
  res = INTRINSIC (_cmp_ph_mask) (HF(src1), HF(src2), 0);
  CHECK_RESULT_MASK (res, exp, N_ELEMS, _cmp_ph_mask);

  exp = EMULATE(cmp_ph) (src1, src2,  MASK_VALUE, 0); 
  res = INTRINSIC (_mask_cmp_ph_mask) (MASK_VALUE, HF(src1), HF(src2), 0);
  CHECK_RESULT_MASK (res, exp, N_ELEMS, _mask_cmp_ph_mask);

#if AVX512F_LEN == 512
  exp = EMULATE(cmp_ph) (src1, src2,  NET_MASK, 0); 
  res = INTRINSIC (_cmp_round_ph_mask) (HF(src1), HF(src2), 0, 8);
  CHECK_RESULT_MASK (res, exp, N_ELEMS, _cmp_round_ph_mask);

  exp = EMULATE(cmp_ph) (src1, src2,  MASK_VALUE, 0);
  res = INTRINSIC (_mask_cmp_round_ph_mask) (MASK_VALUE, HF(src1), HF(src2), 0, 8);
  CHECK_RESULT_MASK (res, exp, N_ELEMS, _mask_cmp_round_ph_mask);
#endif

  if (n_errs != 0) {
      abort ();
  }
}

