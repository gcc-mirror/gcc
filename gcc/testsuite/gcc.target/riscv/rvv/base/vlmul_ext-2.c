/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O0" } */

#include "riscv_vector.h"

void test_vlmul_ext_v_i8mf8_i8mf4(vint8mf8_t op1) {
  vint8mf4_t res = __riscv_vlmul_ext_v_i8mf8_i8mf4(op1);
}
