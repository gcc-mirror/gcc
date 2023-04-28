/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void
f_vfloat32m4x2_t (void *base, void *out)
{
  vfloat32m4x2_t v = *(vfloat32m4x2_t*)base;
  *(vfloat32m4x2_t*)out = v;
}

/* { dg-final { scan-assembler-not {srai} } } */
/* { dg-final { scan-assembler {slli} } } */
/* { dg-final { scan-assembler-times {vl4re32\.v\tv[0-9]+,0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vs4r\.v\tv[0-9]+,0\([a-x0-9]+\)} 2 } } */
