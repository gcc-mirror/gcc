/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (void *in, void *out, int n)
{
  for (int i = 0; i < n; i++)
    {
      asm volatile("nop" ::: "memory");
      vint16m2_t v0 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v0 = __riscv_vwsub_wv_i16m2_tu (v0, v0, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v0, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v1 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v1 = __riscv_vwsub_wv_i16m2_tu (v1, v1, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v1, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v2 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v2 = __riscv_vwsub_wv_i16m2_tu (v2, v2, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v2, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v3 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v3 = __riscv_vwsub_wv_i16m2_tu (v3, v3, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v3, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v4 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v4 = __riscv_vwsub_wv_i16m2_tu (v4, v4, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v4, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v5 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v5 = __riscv_vwsub_wv_i16m2_tu (v5, v5, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v5, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v6 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v6 = __riscv_vwsub_wv_i16m2_tu (v6, v6, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v6, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v7 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v7 = __riscv_vwsub_wv_i16m2_tu (v7, v7, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v7, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v8 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v8 = __riscv_vwsub_wv_i16m2_tu (v8, v8, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v8, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v9 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v9 = __riscv_vwsub_wv_i16m2_tu (v9, v9, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v9, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v10 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v10 = __riscv_vwsub_wv_i16m2_tu (v10, v10, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v10, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v11 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v11 = __riscv_vwsub_wv_i16m2_tu (v11, v11, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v11, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v12 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v12 = __riscv_vwsub_wv_i16m2_tu (v12, v12, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v12, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v13 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v13 = __riscv_vwsub_wv_i16m2_tu (v13, v13, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v13, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v14 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v14 = __riscv_vwsub_wv_i16m2_tu (v14, v14, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v14, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint8m1_t v15_n = __riscv_vle8_v_i8m1 (in, 4);in+=100;
      vint16m2_t v15 = __riscv_vwcvt_x_x_v_i16m2 (v15_n, 4);

      asm volatile("nop" ::: "memory");
      __riscv_vsse16_v_i16m2 (out, 4, v0, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v1, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v2, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v3, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v4, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v5, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v6, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v7, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v8, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v9, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v10, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v11, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v12, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v13, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v14, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v15, 4);out+=100;
    }
}

void
foo2 (void *in, void *out, int n)
{
  for (int i = 0; i < n; i++)
    {
      asm volatile("nop" ::: "memory");
      vint16m2_t v0 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v0 = __riscv_vwadd_wv_i16m2_tu (v0, v0, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v0, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v1 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v1 = __riscv_vwadd_wv_i16m2_tu (v1, v1, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v1, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v2 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v2 = __riscv_vwadd_wv_i16m2_tu (v2, v2, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v2, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v3 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v3 = __riscv_vwadd_wv_i16m2_tu (v3, v3, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v3, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v4 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v4 = __riscv_vwadd_wv_i16m2_tu (v4, v4, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v4, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v5 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v5 = __riscv_vwadd_wv_i16m2_tu (v5, v5, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v5, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v6 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v6 = __riscv_vwadd_wv_i16m2_tu (v6, v6, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v6, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v7 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v7 = __riscv_vwadd_wv_i16m2_tu (v7, v7, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v7, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v8 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v8 = __riscv_vwadd_wv_i16m2_tu (v8, v8, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v8, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v9 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v9 = __riscv_vwadd_wv_i16m2_tu (v9, v9, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v9, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v10 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v10 = __riscv_vwadd_wv_i16m2_tu (v10, v10, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v10, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v11 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v11 = __riscv_vwadd_wv_i16m2_tu (v11, v11, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v11, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v12 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v12 = __riscv_vwadd_wv_i16m2_tu (v12, v12, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v12, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v13 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v13 = __riscv_vwadd_wv_i16m2_tu (v13, v13, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v13, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m2_t v14 = __riscv_vle16_v_i16m2 (in, 4);in+=100;
      v14 = __riscv_vwadd_wv_i16m2_tu (v14, v14, __riscv_vreinterpret_v_i16m1_i8m1 (__riscv_vget_v_i16m2_i16m1 (v14, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint8m1_t v15_n = __riscv_vle8_v_i8m1 (in, 4);in+=100;
      vint16m2_t v15 = __riscv_vwcvt_x_x_v_i16m2 (v15_n, 4);

      asm volatile("nop" ::: "memory");
      __riscv_vsse16_v_i16m2 (out, 4, v0, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v1, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v2, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v3, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v4, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v5, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v6, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v7, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v8, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v9, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v10, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v11, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v12, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v13, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v14, 4);out+=100;
      __riscv_vsse16_v_i16m2 (out, 4, v15, 4);out+=100;
    }
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv4r} } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} { xfail riscv*-*-* } } } */
