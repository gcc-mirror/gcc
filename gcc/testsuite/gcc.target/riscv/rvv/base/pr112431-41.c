/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (void *in, void *out, int n)
{
  for (int i = 0; i < n; i++)
    {
      asm volatile("nop" ::: "memory");
      vint16m8_t v0 = __riscv_vle16_v_i16m8 (in, 4);in+=100;
      v0 = __riscv_vwsub_wv_i16m8_tu (v0, v0, __riscv_vreinterpret_v_i16m4_i8m4 (__riscv_vget_v_i16m8_i16m4 (v0, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m8_t v1 = __riscv_vle16_v_i16m8 (in, 4);in+=100;
      v1 = __riscv_vwsub_wv_i16m8_tu (v1, v1, __riscv_vreinterpret_v_i16m4_i8m4 (__riscv_vget_v_i16m8_i16m4 (v1, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m8_t v2 = __riscv_vle16_v_i16m8 (in, 4);in+=100;
      v2 = __riscv_vwsub_wv_i16m8_tu (v2, v2, __riscv_vreinterpret_v_i16m4_i8m4 (__riscv_vget_v_i16m8_i16m4 (v2, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint8m4_t v3_n = __riscv_vle8_v_i8m4 (in, 4);in+=100;
      vint16m8_t v3 = __riscv_vwcvt_x_x_v_i16m8 (v3_n, 4);

      asm volatile("nop" ::: "memory");
      __riscv_vsse16_v_i16m8 (out, 4, v0, 4);out+=100;
      __riscv_vsse16_v_i16m8 (out, 4, v1, 4);out+=100;
      __riscv_vsse16_v_i16m8 (out, 4, v2, 4);out+=100;
      __riscv_vsse16_v_i16m8 (out, 4, v3, 4);out+=100;
    }
}

void
foo2 (void *in, void *out, int n)
{
  for (int i = 0; i < n; i++)
    {
      asm volatile("nop" ::: "memory");
      vint16m8_t v0 = __riscv_vle16_v_i16m8 (in, 4);in+=100;
      v0 = __riscv_vwadd_wv_i16m8_tu (v0, v0, __riscv_vreinterpret_v_i16m4_i8m4 (__riscv_vget_v_i16m8_i16m4 (v0, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m8_t v1 = __riscv_vle16_v_i16m8 (in, 4);in+=100;
      v1 = __riscv_vwadd_wv_i16m8_tu (v1, v1, __riscv_vreinterpret_v_i16m4_i8m4 (__riscv_vget_v_i16m8_i16m4 (v1, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint16m8_t v2 = __riscv_vle16_v_i16m8 (in, 4);in+=100;
      v2 = __riscv_vwadd_wv_i16m8_tu (v2, v2, __riscv_vreinterpret_v_i16m4_i8m4 (__riscv_vget_v_i16m8_i16m4 (v2, 1)), 4);
      asm volatile("nop" ::: "memory");
      vint8m4_t v3_n = __riscv_vle8_v_i8m4 (in, 4);in+=100;
      vint16m8_t v3 = __riscv_vwcvt_x_x_v_i16m8 (v3_n, 4);

      asm volatile("nop" ::: "memory");
      __riscv_vsse16_v_i16m8 (out, 4, v0, 4);out+=100;
      __riscv_vsse16_v_i16m8 (out, 4, v1, 4);out+=100;
      __riscv_vsse16_v_i16m8 (out, 4, v2, 4);out+=100;
      __riscv_vsse16_v_i16m8 (out, 4, v3, 4);out+=100;
    }
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} } } */
/* { dg-final { scan-assembler-not {vmv8r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {csrr} { xfail riscv*-*-* } } } */
