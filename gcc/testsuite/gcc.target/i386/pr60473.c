/* PR rtl-optimization/60473  */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

unsigned long long foo()
{
  unsigned long long h,l;
  asm volatile ("rdtsc": "=a" (l), "=d" (h));
  return l | (h << 32);
}

/* { dg-final { scan-assembler-not "mov" } } */
