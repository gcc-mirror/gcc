/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-Ofast -mavx512fp16 -mavx512dq" } */

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

  /* Abs for float16.  */
  emulate_absneg_ph (&exp, src1, 1);
  res.f16[0] = __builtin_fabsf16 (src1.f16[0]);
  check_results (&res, &exp, 1, "abs_float16");

  /* Neg for float16.  */
  emulate_absneg_ph (&exp, src1, 0);
  res.f16[0] = -(src1.f16[0]);
  check_results (&res, &exp, 1, "neg_float16");

  /* Copysign for float16.  */
  emulate_copysign_ph (&exp, src1, src2, 0);
  res.f16[0] = __builtin_copysignf16 (src1.f16[0], src2.f16[0]);
  check_results (&res, &exp, 1, "copysign_float16");

  /* Xorsign for float16.  */
  emulate_copysign_ph (&exp, src1, src2, 1);
  res.f16[0] = src1.f16[0] * __builtin_copysignf16 (1, src2.f16[0]);
  check_results (&res, &exp, 1, "xorsign_float16");

  if (n_errs != 0) {
    abort ();
  }
}
