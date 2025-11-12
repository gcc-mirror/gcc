/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d" } */

#include "riscv_vector.h"

int a;
int b();

int c() {
  if (b())
    a = 0;
}

void d() {
  for (; c() + d;) {
    long e, h;
    char *f;
    __rvv_uint16mf4_t g;
    __rvv_uint8mf8x3_t i = __riscv_vlseg3e8ff_v_u8mf8x3(f, &h, e);
    __riscv_vsoxseg3ei16_v_u8mf8x3(0, g, i, 0);
  }
}
