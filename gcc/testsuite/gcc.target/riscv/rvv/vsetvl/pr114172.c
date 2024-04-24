/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void e(long, vfloat32m4_t);

void b(long c) {
  for (;;) {
    c = __riscv_vsetvl_e32m4(c);
    vfloat32m4_t d;
    e(c, d);
  }
}
