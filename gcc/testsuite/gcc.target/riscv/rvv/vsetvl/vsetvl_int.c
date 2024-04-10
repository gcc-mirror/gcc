/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d" } */

#include "riscv_vector.h"

void bar1 (int32_t a);

int32_t
foo1 ()
{
  int32_t a = __riscv_vsetvl_e8mf8(19);
  bar1 (a);
  return a;
}

void bar2 (uint32_t a);

uint32_t
foo2 ()
{
  uint32_t a = __riscv_vsetvl_e8mf8(19);
  bar2 (a);
  return a;
}

int32_t foo3 ()
{
  return __riscv_vsetvl_e8mf8(19);
}

/* { dg-final { scan-assembler-not {sext\.w} { target { no-opts "-O0" "-g" } } } } */
