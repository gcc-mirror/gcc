/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (void *in, void *out, int n)
{
  for (int i = 0; i < n; i++)
    {
      asm volatile("nop" ::: "memory");
      vint16m4_t v0 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v0 = __riscv_vwsub_wv_i16m4_tu (v0, v0, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v0, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v1 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v1 = __riscv_vwsub_wv_i16m4_tu (v1, v1, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v1, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v2 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v2 = __riscv_vwsub_wv_i16m4_tu (v2, v2, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v2, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v3 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v3 = __riscv_vwsub_wv_i16m4_tu (v3, v3, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v3, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v4 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v4 = __riscv_vwsub_wv_i16m4_tu (v4, v4, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v4, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v5 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v5 = __riscv_vwsub_wv_i16m4_tu (v5, v5, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v5, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v6 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v6 = __riscv_vwsub_wv_i16m4_tu (v6, v6, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v6, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint8m2_t v7_n = __riscv_vle8_v_i8m2 (in, 4);in+=100;
      vint16m4_t v7 = __riscv_vwcvt_x_x_v_i16m4 (v7_n, 4);

      asm volatile("nop" ::: "memory");
      __riscv_vsse16_v_i16m4 (out, 4, v0, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v1, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v2, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v3, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v4, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v5, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v6, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v7, 4);out+=100;
    }
}

void
foo2 (void *in, void *out, int n)
{
  for (int i = 0; i < n; i++)
    {
      asm volatile("nop" ::: "memory");
      vint16m4_t v0 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v0 = __riscv_vwadd_wv_i16m4_tu (v0, v0, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v0, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v1 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v1 = __riscv_vwadd_wv_i16m4_tu (v1, v1, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v1, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v2 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v2 = __riscv_vwadd_wv_i16m4_tu (v2, v2, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v2, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v3 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v3 = __riscv_vwadd_wv_i16m4_tu (v3, v3, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v3, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v4 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v4 = __riscv_vwadd_wv_i16m4_tu (v4, v4, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v4, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v5 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v5 = __riscv_vwadd_wv_i16m4_tu (v5, v5, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v5, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m4_t v6 = __riscv_vle16_v_i16m4 (in, 4);in+=100;
      v6 = __riscv_vwadd_wv_i16m4_tu (v6, v6, __riscv_vreinterpret_v_i16m2_i8m2 (__riscv_vget_v_i16m4_i16m2 (v6, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint8m2_t v7_n = __riscv_vle8_v_i8m2 (in, 4);in+=100;
      vint16m4_t v7 = __riscv_vwcvt_x_x_v_i16m4 (v7_n, 4);

      asm volatile("nop" ::: "memory");
      __riscv_vsse16_v_i16m4 (out, 4, v0, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v1, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v2, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v3, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v4, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v5, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v6, 4);out+=100;
      __riscv_vsse16_v_i16m4 (out, 4, v7, 4);out+=100;
    }
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} { xfail riscv*-*-* } } } */
