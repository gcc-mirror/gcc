/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (void *in, void *out, int n)
{
  vuint8m2_t vs = __riscv_vle8_v_u8m2 (in, 4);in+=100;

  for (int i = 0; i < n; i++)
    {
      asm volatile("nop" ::: "memory");
      vuint16m4_t v0 = __riscv_vle16_v_u16m4 (in, 4);in+=100;
      vuint8m2_t v0_n = __riscv_vreinterpret_v_u16m2_u8m2 (__riscv_vget_v_u16m4_u16m2 (v0, 1));
      v0 = __riscv_vwsubu_vv_u16m4_tu (v0, v0_n, vs, 4);
      asm volatile("nop" ::: "memory");
      vuint16m4_t v1 = __riscv_vle16_v_u16m4 (in, 4);in+=100;
      vuint8m2_t v1_n = __riscv_vreinterpret_v_u16m2_u8m2 (__riscv_vget_v_u16m4_u16m2 (v1, 1));
      v1 = __riscv_vwsubu_vv_u16m4_tu (v1, v1_n, vs, 4);
      asm volatile("nop" ::: "memory");
      vuint16m4_t v2 = __riscv_vle16_v_u16m4 (in, 4);in+=100;
      vuint8m2_t v2_n = __riscv_vreinterpret_v_u16m2_u8m2 (__riscv_vget_v_u16m4_u16m2 (v2, 1));
      v2 = __riscv_vwsubu_vv_u16m4_tu (v2, v2_n, vs, 4);
      asm volatile("nop" ::: "memory");
      vuint16m4_t v3 = __riscv_vle16_v_u16m4 (in, 4);in+=100;
      vuint8m2_t v3_n = __riscv_vreinterpret_v_u16m2_u8m2 (__riscv_vget_v_u16m4_u16m2 (v3, 1));
      v3 = __riscv_vwsubu_vv_u16m4_tu (v3, v3_n, vs, 4);
      asm volatile("nop" ::: "memory");
      vuint16m4_t v4 = __riscv_vle16_v_u16m4 (in, 4);in+=100;
      vuint8m2_t v4_n = __riscv_vreinterpret_v_u16m2_u8m2 (__riscv_vget_v_u16m4_u16m2 (v4, 1));
      v4 = __riscv_vwsubu_vv_u16m4_tu (v4, v4_n, vs, 4);
      asm volatile("nop" ::: "memory");
      vuint16m4_t v5 = __riscv_vle16_v_u16m4 (in, 4);in+=100;
      vuint8m2_t v5_n = __riscv_vreinterpret_v_u16m2_u8m2 (__riscv_vget_v_u16m4_u16m2 (v5, 1));
      v5 = __riscv_vwsubu_vv_u16m4_tu (v5, v5_n, vs, 4);
      asm volatile("nop" ::: "memory");
      vuint8m2_t v6_n = __riscv_vle8_v_u8m2 (in, 4);in+=100;
      vuint16m4_t v6 = __riscv_vwcvtu_x_x_v_u16m4 (v6_n, 4);

      asm volatile("nop" ::: "memory");
      __riscv_vsse16_v_u16m4 (out, 4, v0, 4);out+=100;
      __riscv_vsse16_v_u16m4 (out, 4, v1, 4);out+=100;
      __riscv_vsse16_v_u16m4 (out, 4, v2, 4);out+=100;
      __riscv_vsse16_v_u16m4 (out, 4, v3, 4);out+=100;
      __riscv_vsse16_v_u16m4 (out, 4, v4, 4);out+=100;
      __riscv_vsse16_v_u16m4 (out, 4, v5, 4);out+=100;
      __riscv_vsse16_v_u16m4 (out, 4, v6, 4);out+=100;
    }
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
