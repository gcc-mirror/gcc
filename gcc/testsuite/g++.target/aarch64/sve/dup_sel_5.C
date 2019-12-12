/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size(32)));

void
foo (int32_t val)
{
  register vnx4si x asm ("z0");
  val += 1;
  vnx4si y = { val, val, val, val, val, val, val, val };
  x = (vnx4si) { -1, 0, 0, -1, 0, -1, 0, -1 } ? y : (vnx4si) { 0 };
  asm volatile ("" :: "w" (x));
}

/* { dg-final { scan-assembler {\tmovprfx\tz0\.s, p[0-7]/z, z0\.s\n\tmov\tz0\.s, p[0-7]/m, w[0-9]+\n} } } */
