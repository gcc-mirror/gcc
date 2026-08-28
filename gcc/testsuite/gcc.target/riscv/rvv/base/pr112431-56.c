/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (void *in, void *out, int n, int16_t x)
{
  for (int i = 0; i < n; i++)
    {
      asm volatile("nop" ::: "memory");
      vint32m4_t v0 = __riscv_vle32_v_i32m4 (in, 4);in+=100;
      vuint32m4_t v0_u = __riscv_vreinterpret_v_i32m4_u32m4 (v0);
      vuint16m2_t v0_n = __riscv_vreinterpret_v_u32m2_u16m2 (__riscv_vget_v_u32m4_u32m2 (v0_u, 1));
      v0 = __riscv_vwmaccsu_vx_i32m4_tu (v0, x, v0_n, 4);
      asm volatile("nop" ::: "memory");
      vint32m4_t v1 = __riscv_vle32_v_i32m4 (in, 4);in+=100;
      vuint32m4_t v1_u = __riscv_vreinterpret_v_i32m4_u32m4 (v1);
      vuint16m2_t v1_n = __riscv_vreinterpret_v_u32m2_u16m2 (__riscv_vget_v_u32m4_u32m2 (v1_u, 1));
      v1 = __riscv_vwmaccsu_vx_i32m4_tu (v1, x, v1_n, 4);
      asm volatile("nop" ::: "memory");
      vint32m4_t v2 = __riscv_vle32_v_i32m4 (in, 4);in+=100;
      vuint32m4_t v2_u = __riscv_vreinterpret_v_i32m4_u32m4 (v2);
      vuint16m2_t v2_n = __riscv_vreinterpret_v_u32m2_u16m2 (__riscv_vget_v_u32m4_u32m2 (v2_u, 1));
      v2 = __riscv_vwmaccsu_vx_i32m4_tu (v2, x, v2_n, 4);
      asm volatile("nop" ::: "memory");
      vint32m4_t v3 = __riscv_vle32_v_i32m4 (in, 4);in+=100;
      vuint32m4_t v3_u = __riscv_vreinterpret_v_i32m4_u32m4 (v3);
      vuint16m2_t v3_n = __riscv_vreinterpret_v_u32m2_u16m2 (__riscv_vget_v_u32m4_u32m2 (v3_u, 1));
      v3 = __riscv_vwmaccsu_vx_i32m4_tu (v3, x, v3_n, 4);
      asm volatile("nop" ::: "memory");
      vint32m4_t v4 = __riscv_vle32_v_i32m4 (in, 4);in+=100;
      vuint32m4_t v4_u = __riscv_vreinterpret_v_i32m4_u32m4 (v4);
      vuint16m2_t v4_n = __riscv_vreinterpret_v_u32m2_u16m2 (__riscv_vget_v_u32m4_u32m2 (v4_u, 1));
      v4 = __riscv_vwmaccsu_vx_i32m4_tu (v4, x, v4_n, 4);
      asm volatile("nop" ::: "memory");
      vint32m4_t v5 = __riscv_vle32_v_i32m4 (in, 4);in+=100;
      vuint32m4_t v5_u = __riscv_vreinterpret_v_i32m4_u32m4 (v5);
      vuint16m2_t v5_n = __riscv_vreinterpret_v_u32m2_u16m2 (__riscv_vget_v_u32m4_u32m2 (v5_u, 1));
      v5 = __riscv_vwmaccsu_vx_i32m4_tu (v5, x, v5_n, 4);
      asm volatile("nop" ::: "memory");
      vint32m4_t v6 = __riscv_vle32_v_i32m4 (in, 4);in+=100;
      vuint32m4_t v6_u = __riscv_vreinterpret_v_i32m4_u32m4 (v6);
      vuint16m2_t v6_n = __riscv_vreinterpret_v_u32m2_u16m2 (__riscv_vget_v_u32m4_u32m2 (v6_u, 1));
      v6 = __riscv_vwmaccsu_vx_i32m4_tu (v6, x, v6_n, 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v7_n = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      vint32m4_t v7 = __riscv_vwcvt_x_x_v_i32m4 (v7_n, 4);

      asm volatile("nop" ::: "memory");
      __riscv_vsse32_v_i32m4 (out, 4, v0, 4);out+=100;
      __riscv_vsse32_v_i32m4 (out, 4, v1, 4);out+=100;
      __riscv_vsse32_v_i32m4 (out, 4, v2, 4);out+=100;
      __riscv_vsse32_v_i32m4 (out, 4, v3, 4);out+=100;
      __riscv_vsse32_v_i32m4 (out, 4, v4, 4);out+=100;
      __riscv_vsse32_v_i32m4 (out, 4, v5, 4);out+=100;
      __riscv_vsse32_v_i32m4 (out, 4, v6, 4);out+=100;
      __riscv_vsse32_v_i32m4 (out, 4, v7, 4);out+=100;
    }
}

/* The narrowed unsigned source is the highest-numbered half of the signed
   accumulator, thus it overlaps the destination register group.  */
/* { dg-final { scan-assembler-times {vwmaccsu\.vx\s+v4,[^,]+,v6([^0-9]|$)} 7 } } */
/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
