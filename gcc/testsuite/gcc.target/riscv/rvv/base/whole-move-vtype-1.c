/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcv -mabi=lp64d" } */

#include <riscv_vector.h>

/* Returning B needs whole-register copies.  The preceding e32,m1
   configuration is sufficient; do not switch to the tuple's e8,m1.  */
vint8m1x8_t
foo (vint8m8_t a, vint8m1x8_t b, int32_t *out, const int32_t *in, size_t n)
{
  vint32m1_t x = __riscv_vle32_v_i32m1 (in, n);
  __riscv_vse32_v_i32m1 (out, x, n);
  return b;
}

/* { dg-final { scan-assembler-times {vsetvli} 1 } } */
/* { dg-final { scan-assembler-not {vsetivli} } } */
/* { dg-final { scan-assembler-times {vmv1r\.v} 8 } } */
