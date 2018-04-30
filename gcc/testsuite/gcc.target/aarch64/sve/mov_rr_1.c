/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 -msve-vector-bits=256 --save-temps" } */

void sve_copy_rr (void)
{
  typedef int vnx4si __attribute__((vector_size(32)));
  register vnx4si x asm ("z1");
  register vnx4si y asm ("z2");
  asm volatile ("#foo" : "=w" (x));
  y = x;
  asm volatile ("#foo" :: "w" (y));
}

/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
