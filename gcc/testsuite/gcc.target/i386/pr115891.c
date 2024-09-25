/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2" } */

void __attribute__((regparm(1)))
f (int *ptr)
{
  asm volatile ("mem = %0" : "=m" (*ptr) :: "ax");
}

/* { dg-final { scan-assembler-not {mem = [^\n]*%eax} } } */
