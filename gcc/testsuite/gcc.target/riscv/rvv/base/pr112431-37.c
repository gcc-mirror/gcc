/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (void *in, void *out)
{
  vint16m2_t accum = __riscv_vle16_v_i16m2 (in, 4);
  vint16m1_t high_eew16 = __riscv_vget_v_i16m2_i16m1 (accum, 1);
  vint8m1_t high_eew8 = __riscv_vreinterpret_v_i16m1_i8m1 (high_eew16);
  vint16m2_t result = __riscv_vwmacc_vx_i16m2 (accum, 16, high_eew8, 4);
  __riscv_vse16_v_i16m2 (out, result, 4);
}

void
foo2 (void *in, void *out)
{
  vint16m4_t accum = __riscv_vle16_v_i16m4 (in, 4);
  vint16m2_t high_eew16 = __riscv_vget_v_i16m4_i16m2 (accum, 1);
  vint8m2_t high_eew8 = __riscv_vreinterpret_v_i16m2_i8m2 (high_eew16);
  vint16m4_t result = __riscv_vwmacc_vx_i16m4 (accum, 16, high_eew8, 4);
  __riscv_vse16_v_i16m4 (out, result, 4);
}

void
foo3 (void *in, void *out)
{
  vint16m8_t accum = __riscv_vle16_v_i16m8 (in, 4);
  vint16m4_t high_eew16 = __riscv_vget_v_i16m8_i16m4 (accum, 1);
  vint8m4_t high_eew8 = __riscv_vreinterpret_v_i16m4_i8m4 (high_eew16);
  vint16m8_t result = __riscv_vwmacc_vx_i16m8 (accum, 16, high_eew8, 4);
  __riscv_vse16_v_i16m8 (out, result, 4);
}

void
foo4 (void *in, void *out)
{
  vint16m2_t accum = __riscv_vle16_v_i16m2 (in, 4);
  vint16m1_t high_eew16 = __riscv_vget_v_i16m2_i16m1 (accum, 1);
  vint8m1_t high_eew8 = __riscv_vreinterpret_v_i16m1_i8m1 (high_eew16);
  vint16m2_t result = __riscv_vwmaccus_vx_i16m2 (accum, 16, high_eew8, 4);
  __riscv_vse16_v_i16m2 (out, result, 4);
}

void
foo5 (void *in, void *out)
{
  vint16m4_t accum = __riscv_vle16_v_i16m4 (in, 4);
  vint16m2_t high_eew16 = __riscv_vget_v_i16m4_i16m2 (accum, 1);
  vint8m2_t high_eew8 = __riscv_vreinterpret_v_i16m2_i8m2 (high_eew16);
  vint16m4_t result = __riscv_vwmaccus_vx_i16m4 (accum, 16, high_eew8, 4);
  __riscv_vse16_v_i16m4 (out, result, 4);
}

void
foo6 (void *in, void *out)
{
  vint16m8_t accum = __riscv_vle16_v_i16m8 (in, 4);
  vint16m4_t high_eew16 = __riscv_vget_v_i16m8_i16m4 (accum, 1);
  vint8m4_t high_eew8 = __riscv_vreinterpret_v_i16m4_i8m4 (high_eew16);
  vint16m8_t result = __riscv_vwmaccus_vx_i16m8 (accum, 16, high_eew8, 4);
  __riscv_vse16_v_i16m8 (out, result, 4);
}

void
foo7 (void *in, void *out)
{
  vint16m2_t accum = __riscv_vle16_v_i16m2 (in, 4);
  vint16m1_t high_eew16 = __riscv_vget_v_i16m2_i16m1 (accum, 1);
  vint8m1_t high_eew8 = __riscv_vreinterpret_v_i16m1_i8m1 (high_eew16);
  vuint8m1_t high_ueew8 = __riscv_vreinterpret_v_i8m1_u8m1 (high_eew8);
  vint16m2_t result = __riscv_vwmaccsu_vx_i16m2 (accum, 16, high_ueew8, 4);
  __riscv_vse16_v_i16m2 (out, result, 4);
}

void
foo8 (void *in, void *out)
{
  vint16m4_t accum = __riscv_vle16_v_i16m4 (in, 4);
  vint16m2_t high_eew16 = __riscv_vget_v_i16m4_i16m2 (accum, 1);
  vint8m2_t high_eew8 = __riscv_vreinterpret_v_i16m2_i8m2 (high_eew16);
  vuint8m2_t high_ueew8 = __riscv_vreinterpret_v_i8m2_u8m2 (high_eew8);
  vint16m4_t result = __riscv_vwmaccsu_vx_i16m4 (accum, 16, high_ueew8, 4);
  __riscv_vse16_v_i16m4 (out, result, 4);
}

void
foo9 (void *in, void *out)
{
  vint16m8_t accum = __riscv_vle16_v_i16m8 (in, 4);
  vint16m4_t high_eew16 = __riscv_vget_v_i16m8_i16m4 (accum, 1);
  vint8m4_t high_eew8 = __riscv_vreinterpret_v_i16m4_i8m4 (high_eew16);
  vuint8m4_t high_ueew8 = __riscv_vreinterpret_v_i8m4_u8m4 (high_eew8);
  vint16m8_t result = __riscv_vwmaccsu_vx_i16m8 (accum, 16, high_ueew8, 4);
  __riscv_vse16_v_i16m8 (out, result, 4);
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv4r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv8r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {csrr} } } */
