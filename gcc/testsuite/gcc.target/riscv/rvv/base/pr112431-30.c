/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

size_t __attribute__ ((noinline))
sumation (size_t sum0, size_t sum1, size_t sum2, size_t sum3)
{
  return sum0 + sum1 + sum2 + sum3;
}

size_t
foo (char const *buf, size_t len)
{
  size_t sum = 0;
  size_t vl = __riscv_vsetvlmax_e8m8 ();
  size_t step = vl * 4;
  const char *it = buf, *end = buf + len;
  for (; it + step <= end;)
    {
      vuint8m4_t v0 = __riscv_vle8_v_u8m4 ((void *) it, vl);
      it += vl;
      vuint8m4_t v1 = __riscv_vle8_v_u8m4 ((void *) it, vl);
      it += vl;
      vuint8m4_t v2 = __riscv_vle8_v_u8m4 ((void *) it, vl);
      it += vl;
      vuint8m4_t v3 = __riscv_vle8_v_u8m4 ((void *) it, vl);
      it += vl;

      asm volatile("nop" ::: "memory");
      vint16m8_t vw0 = __riscv_vluxei8_v_i16m8 ((void *) it, v0, vl);
      vint16m8_t vw1 = __riscv_vluxei8_v_i16m8 ((void *) it, v1, vl);
      vint16m8_t vw2 = __riscv_vluxei8_v_i16m8 ((void *) it, v2, vl);
      vint16m8_t vw3 = __riscv_vluxei8_v_i16m8 ((void *) it, v3, vl);

      asm volatile("nop" ::: "memory");
      size_t sum0 = __riscv_vmv_x_s_i16m8_i16 (vw0);
      size_t sum1 = __riscv_vmv_x_s_i16m8_i16 (vw1);
      size_t sum2 = __riscv_vmv_x_s_i16m8_i16 (vw2);
      size_t sum3 = __riscv_vmv_x_s_i16m8_i16 (vw3);

      sum += sumation (sum0, sum1, sum2, sum3);
    }
  return sum;
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} { xfail riscv*-*-* } } } */
