/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size(32)));
typedef float vnx4sf __attribute__((vector_size(32)));

void
foo (float val)
{
  register vnx4sf x asm ("z0");
  register vnx4sf y asm ("z0");
  asm volatile ("" : "=w" (y));
  vnx4sf z = { val, val, val, val, val, val, val, val };
  x = (vnx4si) { -1, 0, 0, -1, 0, -1, 0, -1 } ? z : y;
  asm volatile ("" :: "w" (x));
}

/* { dg-final { scan-assembler {\tmov\tz0\.s, p[0-7]/m, s[0-9]+\n} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
