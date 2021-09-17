/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */

#define AVX512FP16
#include "avx512fp16-helper.h"

void NOINLINE
emulate_mov2_load_sh(V512 * dest, V512 op1,
		     __mmask8 k, int zero_mask)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;

  unpack_ph_2twops(op1, &v1, &v2);
  unpack_ph_2twops(*dest, &v7, &v8);

  if ((k&1) || !k)
    v5.f32[0] = v1.f32[0];
  else if (zero_mask)
    v5.f32[0] = 0;
  else
    v5.f32[0] = v7.f32[0]; //remains unchanged

  for (i = 1; i < 8; i++)
    v5.f32[i] = 0;

  *dest = pack_twops_2ph(v5, v6);
}

void NOINLINE
emulate_mov3_load_sh(V512 * dest, V512 op1, V512 op2,
		     __mmask8 k, int zero_mask)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;

  unpack_ph_2twops(op1, &v1, &v2);
  unpack_ph_2twops(op2, &v3, &v4);
  unpack_ph_2twops(*dest, &v7, &v8);

  if ((k&1) || !k)
    v5.f32[0] = v3.f32[0];
  else if (zero_mask)
    v5.f32[0] = 0;
  else
    v5.f32[0] = v7.f32[0]; //remains unchanged

  for (i = 1; i < 8; i++)
    v5.f32[i] = v1.f32[i];

  *dest = pack_twops_2ph(v5, v6);
}

void NOINLINE
emulate_mov2_store_sh(V512 * dest, V512 op1, __mmask8 k)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;

  unpack_ph_2twops(op1, &v1, &v2);
  unpack_ph_2twops(*dest, &v7, &v8);

  if ((k&1) || !k)
    v5.f32[0] = v1.f32[0];
  else
    v5.f32[0] = v7.f32[0]; //remains unchanged

  *dest = pack_twops_2ph(v5, v6);
}

void
test_512 (void)
{
  V512 res;
  V512 exp;

  init_src();

  // no mask
  emulate_mov2_load_sh (&exp, src1, 0x0, 0);
  res.xmmh[0] = _mm_load_sh((const void *)&(src1.u16[0]));
  check_results(&res, &exp, 8, "_mm_load_sh");

  // with mask and mask bit is set
  emulate_mov2_load_sh (&exp, src1, 0x1, 0);
  res.xmmh[0] = _mm_mask_load_sh(res.xmmh[0], 0x1, (const void *)&(src1.u16[0]));
  check_results(&res, &exp, 8, "_mm__mask_load_sh");

  // with zero-mask
  emulate_mov2_load_sh (&exp, src1, 0x0, 1);
  res.xmmh[0] = _mm_maskz_load_sh(0x1, (const void *)&(src1.u16[0]));
  check_results(&res, &exp, 8, "_mm_maskz_load_sh");

  emulate_mov3_load_sh (&exp, src1, src2, 0x1, 0);
  res.xmmh[0] = _mm_mask_move_sh(res.xmmh[0], 0x1, src1.xmmh[0], src2.xmmh[0]);
  check_results(&res, &exp, 8, "_mm_mask_move_sh");

  emulate_mov3_load_sh (&exp, src1, src2, 0x1, 1);
  res.xmmh[0] = _mm_maskz_move_sh(0x1, src1.xmmh[0], src2.xmmh[0]);
  check_results(&res, &exp, 8, "_mm_maskz_move_sh");

  // no mask
  emulate_mov2_store_sh (&exp, src1, 0x0);
  _mm_store_sh((void *)&(res.u16[0]), src1.xmmh[0]);
  check_results(&exp, &res, 1, "_mm_store_sh");

  // with mask
  emulate_mov2_store_sh (&exp, src1, 0x1);
  _mm_mask_store_sh((void *)&(res.u16[0]), 0x1, src1.xmmh[0]);
  check_results(&exp, &res, 1, "_mm_mask_store_sh");

  if (n_errs != 0) {
      abort ();
  }
}
