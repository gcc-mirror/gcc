/* { dg-do compile } */
/* { dg-options "-O -msve-vector-bits=1024" } */

typedef int vnx4si __attribute__((vector_size (128)));

void
foo (void)
{
  register int x asm ("z0");
  register vnx4si y asm ("z1");

  asm volatile ("" : "=w" (y));
  x = y[21];
  asm volatile ("" :: "w" (x));
}

/* { dg-final { scan-assembler {\tmovprfx\tz0, z1\n\text\tz0\.b, z0\.b, z1\.b, #84\n} } } */
