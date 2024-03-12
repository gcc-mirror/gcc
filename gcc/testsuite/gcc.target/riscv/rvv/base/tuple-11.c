/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void
f_vint16m4x2_t (void *base, void *out)
{
  vint16m4x2_t v = *(vint16m4x2_t*)base;
  *(vint16m4x2_t*)out = v;
}

void
f_vuint16m4x2_t (void *base, void *out)
{
  vuint16m4x2_t v = *(vuint16m4x2_t*)base;
  *(vuint16m4x2_t*)out = v;
}

/* { dg-final { scan-assembler-not {srai} } } */
/* { dg-final { scan-assembler {slli} } } */
/* { dg-final { scan-assembler-times {vl4re16\.v\tv[0-9]+,0\([a-x0-9]+\)} 4 } } */
/* { dg-final { scan-assembler-times {vs4r\.v\tv[0-9]+,0\([a-x0-9]+\)} 4 } } */
