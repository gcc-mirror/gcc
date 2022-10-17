/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS 8

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

void NOINLINE
emulate_reduce_sh(V512 * dest, V512 op1,
                  __mmask32 k, int imm8, int zero_mask) 
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  V512 t1;
  int i;

  unpack_ph_2twops(op1, &v1, &v2);
  unpack_ph_2twops(*dest, &v7, &v8);
  t1 = borrow_reduce_ps(v1, imm8);

  if ((k&1) || !k)
    v5.f32[0] = t1.f32[0];
  else if (zero_mask)
    v5.f32[0] = 0;
  else
    v5.f32[0] = v7.f32[0];

  for (i = 1; i < 8; i++)
    v5.f32[i] = v1.f32[i];

  *dest = pack_twops_2ph(v5, v6);
}

void
test_512 (void)
{
  V512 res;
  V512 exp;

  init_src();

  emulate_reduce_sh(&exp, src1,  0x1, 8, 0);
  res.xmmh[0] = _mm_reduce_round_sh(src1.xmmh[0], exp.xmmh[0], 8, _ROUND_CUR);
  check_results(&res, &exp, N_ELEMS, "_mm_reduce_round_sh");

  init_dest(&res, &exp);
  emulate_reduce_sh(&exp, src1,  0x1, 7, 0);
  res.xmmh[0] = _mm_mask_reduce_round_sh(res.xmmh[0], 0x1, src1.xmmh[0], exp.xmmh[0], 7, _ROUND_CUR);
  check_results(&res, &exp, N_ELEMS, "_mm_mask_reduce_round_sh");

  emulate_reduce_sh(&exp, src1,  0x3, 6, 1);
  res.xmmh[0] = _mm_maskz_reduce_round_sh(0x3, src1.xmmh[0], exp.xmmh[0], 6, _ROUND_CUR);
  check_results(&res, &exp, N_ELEMS, "_mm_maskz_reduce_round_sh");


  if (n_errs != 0) {
      abort ();
  }
}

