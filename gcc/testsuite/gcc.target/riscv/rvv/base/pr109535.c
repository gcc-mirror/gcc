/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void foo(void *in1, void *in2, void *in3, void *out, size_t vl) {
  vint8m1_t a = __riscv_vle8_v_i8m1(in1, vl);
  vint8m1_t b = __riscv_vadd_vx_i8m1 (a, vl, vl);
  __riscv_vse8_v_i8m1(out, b, vl);
}

