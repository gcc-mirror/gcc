/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=256" } */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size(32)));
typedef float vnx4sf __attribute__((vector_size(32)));

void
foo (float val)
{
  register vnx4sf x asm ("z0");
  vnx4sf y = { val, val, val, val, val, val, val, val };
  x = (vnx4si) { -1, 0, 0, -1, 0, -1, 0, -1 } ? y : (vnx4sf) { 0 };
  asm volatile ("" :: "w" (x));
}

/* { dg-final { scan-assembler {\tmovprfx\tz0\.s, p[0-7]/z, z0\.s\n\tmov\tz0\.s, p[0-7]/m, s[0-9]+\n} } } */
