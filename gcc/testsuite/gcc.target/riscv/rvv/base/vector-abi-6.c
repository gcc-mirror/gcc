/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */
#include "riscv_vector.h"

void
foo(int32_t *in1, int32_t *in2, int32_t *in3, int32_t *out,
    size_t n, int cond) {
  size_t vl;
  if (cond)
    vl = __riscv_vsetvlmax_e32m1();
  else
    vl = __riscv_vsetvlmax_e16mf2();
  for (size_t i = 0; i < n; i += 1)
    {
      vint32m1_t a = __riscv_vle32_v_i32m1(in1, vl); /* { dg-bogus "the vector type" } */
      vint32m1_t b = __riscv_vle32_v_i32m1_tu(a, in2, vl);
      vint32m1_t c = __riscv_vle32_v_i32m1_tu(b, in3, vl);
      __riscv_vse32_v_i32m1(out, c, vl);
    }
}
