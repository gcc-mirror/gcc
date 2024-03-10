/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv_zvfh -mabi=ilp32d" } */

#include "riscv_vector.h"

void
f_vfloat16m4x2_t (void *base, void *out)
{
  vfloat16m4x2_t v = *(vfloat16m4x2_t*)base;
  *(vfloat16m4x2_t*)out = v;
}

/* { dg-final { scan-assembler-not {srai} } } */
/* { dg-final { scan-assembler {slli} } } */
/* { dg-final { scan-assembler-times {vl4re16\.v\tv[0-9]+,0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vs4r\.v\tv[0-9]+,0\([a-x0-9]+\)} 2 } } */