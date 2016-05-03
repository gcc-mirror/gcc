/* { dg-do assemble { target { avx512f && { ! ia32 } } } } */
/* { dg-options "-O2 -mavx512f -mfpmath=387,sse" } */

void
f1 (double *p)
{
  register float x __asm ("xmm16");
  x = *p;
  __asm volatile ("" : "+v" (x));
}

void
f2 (void)
{
  double d;
  register float x __asm ("xmm16");
  __asm volatile ("" : "=t" (d));
  x = d;
  __asm volatile ("" : "+v" (x));
}

void
f3 (long double *p)
{
  register float x __asm ("xmm16");
  x = *p;
  __asm volatile ("" : "+v" (x));
}

void
f4 (void)
{
  long double d;
  register float x __asm ("xmm16");
  __asm volatile ("" : "=t" (d));
  x = d;
  __asm volatile ("" : "+v" (x));
}
