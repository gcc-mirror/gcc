/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (void *in, void *out)
{
  vfloat64m2_t accum = __riscv_vle64_v_f64m2 (in, 4);
  vfloat64m1_t high_eew64 = __riscv_vget_v_f64m2_f64m1 (accum, 1);
  vint64m1_t high_eew64_i = __riscv_vreinterpret_v_f64m1_i64m1 (high_eew64);
  vint32m1_t high_eew32_i = __riscv_vreinterpret_v_i64m1_i32m1 (high_eew64_i);
  vfloat32m1_t high_eew32 = __riscv_vreinterpret_v_i32m1_f32m1 (high_eew32_i);
  vfloat64m2_t result = __riscv_vfwmacc_vf_f64m2 (accum, 64, high_eew32, 4);
  __riscv_vse64_v_f64m2 (out, result, 4);
}

void
foo2 (void *in, void *out)
{
  vfloat64m4_t accum = __riscv_vle64_v_f64m4 (in, 4);
  vfloat64m2_t high_eew64 = __riscv_vget_v_f64m4_f64m2 (accum, 1);
  vint64m2_t high_eew64_i = __riscv_vreinterpret_v_f64m2_i64m2 (high_eew64);
  vint32m2_t high_eew32_i = __riscv_vreinterpret_v_i64m2_i32m2 (high_eew64_i);
  vfloat32m2_t high_eew32 = __riscv_vreinterpret_v_i32m2_f32m2 (high_eew32_i);
  vfloat64m4_t result = __riscv_vfwmacc_vf_f64m4 (accum, 64, high_eew32, 4);
  __riscv_vse64_v_f64m4 (out, result, 4);
}

void
foo3 (void *in, void *out)
{
  vfloat64m8_t accum = __riscv_vle64_v_f64m8 (in, 4);
  vfloat64m4_t high_eew64 = __riscv_vget_v_f64m8_f64m4 (accum, 1);
  vint64m4_t high_eew64_i = __riscv_vreinterpret_v_f64m4_i64m4 (high_eew64);
  vint32m4_t high_eew32_i = __riscv_vreinterpret_v_i64m4_i32m4 (high_eew64_i);
  vfloat32m4_t high_eew32 = __riscv_vreinterpret_v_i32m4_f32m4 (high_eew32_i);
  vfloat64m8_t result = __riscv_vfwmacc_vf_f64m8 (accum, 64, high_eew32, 4);
  __riscv_vse64_v_f64m8 (out, result, 4);
}

void
foo4 (void *in, void *out)
{
  vfloat64m2_t accum = __riscv_vle64_v_f64m2 (in, 4);
  vfloat64m1_t high_eew64 = __riscv_vget_v_f64m2_f64m1 (accum, 1);
  vint64m1_t high_eew64_i = __riscv_vreinterpret_v_f64m1_i64m1 (high_eew64);
  vint32m1_t high_eew32_i = __riscv_vreinterpret_v_i64m1_i32m1 (high_eew64_i);
  vfloat32m1_t high_eew32 = __riscv_vreinterpret_v_i32m1_f32m1 (high_eew32_i);
  vfloat64m2_t result = __riscv_vfwnmsac_vf_f64m2 (accum, 64, high_eew32, 4);
  __riscv_vse64_v_f64m2 (out, result, 4);
}

void
foo5 (void *in, void *out)
{
  vfloat64m4_t accum = __riscv_vle64_v_f64m4 (in, 4);
  vfloat64m2_t high_eew64 = __riscv_vget_v_f64m4_f64m2 (accum, 1);
  vint64m2_t high_eew64_i = __riscv_vreinterpret_v_f64m2_i64m2 (high_eew64);
  vint32m2_t high_eew32_i = __riscv_vreinterpret_v_i64m2_i32m2 (high_eew64_i);
  vfloat32m2_t high_eew32 = __riscv_vreinterpret_v_i32m2_f32m2 (high_eew32_i);
  vfloat64m4_t result = __riscv_vfwnmsac_vf_f64m4 (accum, 64, high_eew32, 4);
  __riscv_vse64_v_f64m4 (out, result, 4);
}

void
foo6 (void *in, void *out)
{
  vfloat64m8_t accum = __riscv_vle64_v_f64m8 (in, 4);
  vfloat64m4_t high_eew64 = __riscv_vget_v_f64m8_f64m4 (accum, 1);
  vint64m4_t high_eew64_i = __riscv_vreinterpret_v_f64m4_i64m4 (high_eew64);
  vint32m4_t high_eew32_i = __riscv_vreinterpret_v_i64m4_i32m4 (high_eew64_i);
  vfloat32m4_t high_eew32 = __riscv_vreinterpret_v_i32m4_f32m4 (high_eew32_i);
  vfloat64m8_t result = __riscv_vfwnmsac_vf_f64m8 (accum, 64, high_eew32, 4);
  __riscv_vse64_v_f64m8 (out, result, 4);
}

/* { dg-final { scan-assembler-not {vmv1r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv2r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv4r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv8r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {csrr} } } */
