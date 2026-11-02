/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (void *in, void *out, int n)
{
  for (int i = 0; i < n; i++)
    {
      asm volatile("nop" ::: "memory");
      vfloat32m4_t v0 = __riscv_vle32_v_f32m4 (in, 4);in+=100;
      vfloat64m1_t v0_w = __riscv_vget_v_f64m4_f64m1 (__riscv_vreinterpret_v_i64m4_f64m4 (__riscv_vreinterpret_v_i32m4_i64m4 (__riscv_vreinterpret_v_f32m4_i32m4 (v0))), 3);
      vfloat64m1_t r0 = __riscv_vfwredosum_vs_f32m4_f64m1 (v0, v0_w, 4);
      asm volatile("nop" ::: "memory");
      vfloat32m4_t v1 = __riscv_vle32_v_f32m4 (in, 4);in+=100;
      vfloat64m1_t v1_w = __riscv_vget_v_f64m4_f64m1 (__riscv_vreinterpret_v_i64m4_f64m4 (__riscv_vreinterpret_v_i32m4_i64m4 (__riscv_vreinterpret_v_f32m4_i32m4 (v1))), 3);
      vfloat64m1_t r1 = __riscv_vfwredosum_vs_f32m4_f64m1 (v1, v1_w, 4);
      asm volatile("nop" ::: "memory");
      vfloat32m4_t v2 = __riscv_vle32_v_f32m4 (in, 4);in+=100;
      vfloat64m1_t v2_w = __riscv_vget_v_f64m4_f64m1 (__riscv_vreinterpret_v_i64m4_f64m4 (__riscv_vreinterpret_v_i32m4_i64m4 (__riscv_vreinterpret_v_f32m4_i32m4 (v2))), 3);
      vfloat64m1_t r2 = __riscv_vfwredosum_vs_f32m4_f64m1 (v2, v2_w, 4);
      asm volatile("nop" ::: "memory");
      vfloat32m4_t v3 = __riscv_vle32_v_f32m4 (in, 4);in+=100;
      vfloat64m1_t v3_w = __riscv_vget_v_f64m4_f64m1 (__riscv_vreinterpret_v_i64m4_f64m4 (__riscv_vreinterpret_v_i32m4_i64m4 (__riscv_vreinterpret_v_f32m4_i32m4 (v3))), 3);
      vfloat64m1_t r3 = __riscv_vfwredosum_vs_f32m4_f64m1 (v3, v3_w, 4);
      asm volatile("nop" ::: "memory");
      vfloat32m4_t v4 = __riscv_vle32_v_f32m4 (in, 4);in+=100;
      vfloat64m1_t v4_w = __riscv_vget_v_f64m4_f64m1 (__riscv_vreinterpret_v_i64m4_f64m4 (__riscv_vreinterpret_v_i32m4_i64m4 (__riscv_vreinterpret_v_f32m4_i32m4 (v4))), 3);
      vfloat64m1_t r4 = __riscv_vfwredosum_vs_f32m4_f64m1 (v4, v4_w, 4);
      asm volatile("nop" ::: "memory");
      vfloat32m4_t v5 = __riscv_vle32_v_f32m4 (in, 4);in+=100;
      vfloat64m1_t v5_w = __riscv_vget_v_f64m4_f64m1 (__riscv_vreinterpret_v_i64m4_f64m4 (__riscv_vreinterpret_v_i32m4_i64m4 (__riscv_vreinterpret_v_f32m4_i32m4 (v5))), 3);
      vfloat64m1_t r5 = __riscv_vfwredosum_vs_f32m4_f64m1 (v5, v5_w, 4);
      asm volatile("nop" ::: "memory");
      vfloat32m4_t v6 = __riscv_vle32_v_f32m4 (in, 4);in+=100;
      vfloat64m1_t v6_w = __riscv_vget_v_f64m4_f64m1 (__riscv_vreinterpret_v_i64m4_f64m4 (__riscv_vreinterpret_v_i32m4_i64m4 (__riscv_vreinterpret_v_f32m4_i32m4 (v6))), 3);
      vfloat64m1_t r6 = __riscv_vfwredosum_vs_f32m4_f64m1 (v6, v6_w, 4);
      asm volatile("nop" ::: "memory");
      vfloat32m4_t v7 = __riscv_vle32_v_f32m4 (in, 4);in+=100;
      vfloat64m1_t r7 = __riscv_vfwredosum_vs_f32m4_f64m1 (v7, __riscv_vfmv_v_f_f64m1 (0.0, 4), 4);

      asm volatile("nop" ::: "memory");
      __riscv_vse64_v_f64m1 (out, r0, 4);out+=100;
      __riscv_vse64_v_f64m1 (out, r1, 4);out+=100;
      __riscv_vse64_v_f64m1 (out, r2, 4);out+=100;
      __riscv_vse64_v_f64m1 (out, r3, 4);out+=100;
      __riscv_vse64_v_f64m1 (out, r4, 4);out+=100;
      __riscv_vse64_v_f64m1 (out, r5, 4);out+=100;
      __riscv_vse64_v_f64m1 (out, r6, 4);out+=100;
      __riscv_vse64_v_f64m1 (out, r7, 4);out+=100;
    }
}

/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-not {vfwredosum\.vs\s+v[0-9]+,(v0,v[0-3]|v4,v[4-7]|v8,v([89]|1[01])|v12,v1[2-5]|v16,v1[6-9]|v20,v2[0-3]|v24,v2[4-7]|v28,v(2[89]|3[01]))([^0-9]|$)} } } */
