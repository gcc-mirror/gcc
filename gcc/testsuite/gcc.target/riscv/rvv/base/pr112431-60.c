/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (void *in, void *out, int n)
{
  for (int i = 0; i < n; i++)
    {
      asm volatile("nop" ::: "memory");
      vfloat64m4_t w = __riscv_vle64_v_f64m4 (in, 4);in+=100;
      asm volatile("nop" ::: "memory");
      vfloat64m4_t v0 = __riscv_vle64_v_f64m4 (in, 4);in+=100;
      vfloat32m2_t v0_n = __riscv_vreinterpret_v_i32m2_f32m2 (__riscv_vreinterpret_v_i64m2_i32m2 (__riscv_vreinterpret_v_f64m2_i64m2 (__riscv_vget_v_f64m4_f64m2 (v0, 1))));
      v0 = __riscv_vfwsub_wv_f64m4_tu (v0, w, v0_n, 4);
      asm volatile("nop" ::: "memory");
      vfloat64m4_t v1 = __riscv_vle64_v_f64m4 (in, 4);in+=100;
      vfloat32m2_t v1_n = __riscv_vreinterpret_v_i32m2_f32m2 (__riscv_vreinterpret_v_i64m2_i32m2 (__riscv_vreinterpret_v_f64m2_i64m2 (__riscv_vget_v_f64m4_f64m2 (v1, 1))));
      v1 = __riscv_vfwsub_wv_f64m4_tu (v1, w, v1_n, 4);
      asm volatile("nop" ::: "memory");
      vfloat64m4_t v2 = __riscv_vle64_v_f64m4 (in, 4);in+=100;
      vfloat32m2_t v2_n = __riscv_vreinterpret_v_i32m2_f32m2 (__riscv_vreinterpret_v_i64m2_i32m2 (__riscv_vreinterpret_v_f64m2_i64m2 (__riscv_vget_v_f64m4_f64m2 (v2, 1))));
      v2 = __riscv_vfwsub_wv_f64m4_tu (v2, w, v2_n, 4);
      asm volatile("nop" ::: "memory");
      vfloat64m4_t v3 = __riscv_vle64_v_f64m4 (in, 4);in+=100;
      vfloat32m2_t v3_n = __riscv_vreinterpret_v_i32m2_f32m2 (__riscv_vreinterpret_v_i64m2_i32m2 (__riscv_vreinterpret_v_f64m2_i64m2 (__riscv_vget_v_f64m4_f64m2 (v3, 1))));
      v3 = __riscv_vfwsub_wv_f64m4_tu (v3, w, v3_n, 4);
      asm volatile("nop" ::: "memory");
      vfloat64m4_t v4 = __riscv_vle64_v_f64m4 (in, 4);in+=100;
      vfloat32m2_t v4_n = __riscv_vreinterpret_v_i32m2_f32m2 (__riscv_vreinterpret_v_i64m2_i32m2 (__riscv_vreinterpret_v_f64m2_i64m2 (__riscv_vget_v_f64m4_f64m2 (v4, 1))));
      v4 = __riscv_vfwsub_wv_f64m4_tu (v4, w, v4_n, 4);
      asm volatile("nop" ::: "memory");
      vfloat64m4_t v5 = __riscv_vle64_v_f64m4 (in, 4);in+=100;
      vfloat32m2_t v5_n = __riscv_vreinterpret_v_i32m2_f32m2 (__riscv_vreinterpret_v_i64m2_i32m2 (__riscv_vreinterpret_v_f64m2_i64m2 (__riscv_vget_v_f64m4_f64m2 (v5, 1))));
      v5 = __riscv_vfwsub_wv_f64m4_tu (v5, w, v5_n, 4);
      asm volatile("nop" ::: "memory");
      vfloat64m4_t v6 = __riscv_vle64_v_f64m4 (in, 4);in+=100;
      vfloat32m2_t v6_n = __riscv_vreinterpret_v_i32m2_f32m2 (__riscv_vreinterpret_v_i64m2_i32m2 (__riscv_vreinterpret_v_f64m2_i64m2 (__riscv_vget_v_f64m4_f64m2 (v6, 1))));
      v6 = __riscv_vfwsub_wv_f64m4_tu (v6, w, v6_n, 4);
      asm volatile("nop" ::: "memory");
      vfloat32m2_t v7_n = __riscv_vle32_v_f32m2 (in, 4);in+=100;
      vfloat64m4_t v7 = __riscv_vfwcvt_f_f_v_f64m4 (v7_n, 4);

      asm volatile("nop" ::: "memory");
      __riscv_vsse64_v_f64m4 (out, 8, v0, 4);out+=100;
      __riscv_vsse64_v_f64m4 (out, 8, v1, 4);out+=100;
      __riscv_vsse64_v_f64m4 (out, 8, v2, 4);out+=100;
      __riscv_vsse64_v_f64m4 (out, 8, v3, 4);out+=100;
      __riscv_vsse64_v_f64m4 (out, 8, v4, 4);out+=100;
      __riscv_vsse64_v_f64m4 (out, 8, v5, 4);out+=100;
      __riscv_vsse64_v_f64m4 (out, 8, v6, 4);out+=100;
      __riscv_vsse64_v_f64m4 (out, 8, v7, 4);out+=100;
    }
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
