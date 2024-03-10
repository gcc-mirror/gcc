/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void
f_vint8m4x2_t (void *base, void *out)
{
  vint8m4x2_t v = *(vint8m4x2_t*)base;
  *(vint8m4x2_t*)out = v;
}

void
f_vuint8m4x2_t (void *base, void *out)
{
  vuint8m4x2_t v = *(vuint8m4x2_t*)base;
  *(vuint8m4x2_t*)out = v;
}

/* { dg-final { scan-assembler-not {srai} } } */
/* { dg-final { scan-assembler {slli} } } */
/* { dg-final { scan-assembler-times {vl4re8\.v\tv[0-9]+,0\([a-x0-9]+\)} 4 } } */
/* { dg-final { scan-assembler-times {vs4r\.v\tv[0-9]+,0\([a-x0-9]+\)} 4 } } */
