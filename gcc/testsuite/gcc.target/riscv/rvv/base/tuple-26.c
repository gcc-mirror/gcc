/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void
f_vint8mf8x2_t (void *out)
{
  vint8mf8x2_t v;
  *(vint8mf8x2_t*)out = v;
}

void
f_vint8m1x2_t (void *out)
{
  vint8m1x2_t v;
  *(vint8m1x2_t*)out = v;
}

void
f_vfloat32mf2x8_t (void *base, void *out)
{
  vfloat32mf2x8_t v;
  *(vfloat32mf2x8_t*)out = v;
}

void
f_vfloat64m4x2_t (void *base, void *out)
{
  vfloat64m4x2_t v;
  *(vfloat64m4x2_t*)out = v;
}

/* { dg-final { scan-assembler-times {vmv.v.i\tv[0-9]+,\s*0} 4 } } */
