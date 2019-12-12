/* { dg-do compile } */
/* { dg-options "-O -msve-vector-bits=256" } */

typedef int vnx4si __attribute__((vector_size (32)));

void
foo (void)
{
  register vnx4si x asm ("z0");
  register vnx4si y asm ("z1");

  asm volatile ("" : "=w" (y));
  x = __builtin_shuffle (y, y, (vnx4si) { 1, 2, 3, 4, 5, 6, 7, 8 });
  asm volatile ("" :: "w" (x));
}

/* { dg-final { scan-assembler {\tmovprfx\tz0, z1\n\text\tz0\.b, z0\.b, z1\.b, #4\n} } } */
