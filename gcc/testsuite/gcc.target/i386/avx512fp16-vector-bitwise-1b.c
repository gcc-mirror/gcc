/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-Ofast -mavx512fp16 -mavx512vl -mavx512dq" } */

#define AVX512FP16
#include "avx512fp16-helper.h"

void NOINLINE
emulate_absneg_ph (V512 * dest, V512 op1, int abs)
{
  V512 v1, v2, v3, v4;
  int i;

  unpack_ph_2twops(op1, &v1, &v2);
  unpack_ph_2twops(*dest, &v3, &v4);

  for (i = 0; i != 16; i++) {
    if (abs) {
      v3.f32[i] = __builtin_fabsf (v1.f32[i]);
      v4.f32[i] = __builtin_fabsf (v2.f32[i]);
    }
    else {
      v3.f32[i] = -v1.f32[i];
      v4.f32[i] = -v2.f32[i];
    }
  }
  *dest = pack_twops_2ph(v3, v4);
}

void NOINLINE
emulate_copysign_ph (V512 * dest, V512 op1, V512 op2, int xorsign)
{
  V512 v1, v2, v3, v4, v5, v6;
  int i;

  unpack_ph_2twops(op1, &v1, &v2);
  unpack_ph_2twops(op2, &v3, &v4);
  unpack_ph_2twops(*dest, &v5, &v6);

  for (i = 0; i != 16; i++) {
    if (xorsign) {
      v5.f32[i] = v1.f32[i] * __builtin_copysignf (1, v3.f32[i]);
      v6.f32[i] = v2.f32[i] * __builtin_copysignf (1, v4.f32[i]);
    }
    else {
      v5.f32[i] = __builtin_copysignf (v1.f32[i], v3.f32[i]);
      v6.f32[i] = __builtin_copysignf (v2.f32[i], v4.f32[i]);
    }
  }
  *dest = pack_twops_2ph(v5, v6);
}


void
test_512 (void)
{
  V512 res, exp;

  init_src ();

  /* Abs for vector float16.  */
  emulate_absneg_ph (&exp, src1, 1);
  for (int i = 0; i != 8; i++)
    res.f16[i] = __builtin_fabsf16 (src1.f16[i]);
  check_results (&res, &exp, 8, "abs_m128h");

  for (int i = 0; i != 16; i++)
    res.f16[i] = __builtin_fabsf16 (src1.f16[i]);
  check_results (&res, &exp, 16, "abs_m256h");

  for (int i = 0; i != 32; i++)
    res.f16[i] = __builtin_fabsf16 (src1.f16[i]);
  check_results (&res, &exp, 32, "abs_m512h");

  /* Neg for vector float16.  */
  emulate_absneg_ph (&exp, src1, 0);
  for (int i = 0; i != 8; i++)
    res.f16[i] = -(src1.f16[i]);
  check_results (&res, &exp, 8, "neg_m128h");

  for (int i = 0; i != 16; i++)
    res.f16[i] = -(src1.f16[i]);
  check_results (&res, &exp, 16, "neg_m256h");

  for (int i = 0; i != 32; i++)
    res.f16[i] = -(src1.f16[i]);
  check_results (&res, &exp, 32, "neg_m512h");

  /* Copysign for vector float16.  */
  emulate_copysign_ph (&exp, src1, src2, 0);
  for (int i = 0; i != 8; i++)
    res.f16[i] = __builtin_copysignf16 (src1.f16[i], src2.f16[i]);
  check_results (&res, &exp, 8, "copysign_m128h");

  for (int i = 0; i != 16; i++)
    res.f16[i] = __builtin_copysignf16 (src1.f16[i], src2.f16[i]);
  check_results (&res, &exp, 16, "copysign_m256h");

  for (int i = 0; i != 32; i++)
    res.f16[i] = __builtin_copysignf16 (src1.f16[i], src2.f16[i]);
  check_results (&res, &exp, 32, "copysign_m512h");

  /* Xorsign for vector float16.  */
  emulate_copysign_ph (&exp, src1, src2, 1);
  for (int i = 0; i != 8; i++)
    res.f16[i] = src1.f16[i] * __builtin_copysignf16 (1, src2.f16[i]);
  check_results (&res, &exp, 8, "xorsign_m128h");

  for (int i = 0; i != 16; i++)
    res.f16[i] = src1.f16[i] * __builtin_copysignf16 (1, src2.f16[i]);
  check_results (&res, &exp, 16, "xorsign_m256h");

  for (int i = 0; i != 32; i++)
    res.f16[i] = src1.f16[i] * __builtin_copysignf16 (1, src2.f16[i]);
  check_results (&res, &exp, 32, "xorsign_m512h");

  if (n_errs != 0) {
    abort ();
  }
}
