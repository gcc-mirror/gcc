/* { dg-options "-O2 -fPIC -msve-vector-bits=256" } */
/* { dg-require-effective-target fpic } */

typedef unsigned int v8si __attribute__((vector_size(32)));

extern __thread int y;

void
f (int *a)
{
  v8si x;
  asm volatile ("dup %0.s, #0x11" : "=w" (x) :: "memory");
  if (*a)
    asm volatile ("insr %0.s, %w2" : "=w" (x) : "0" (x), "r" (y));
}

/* { dg-final { scan-assembler {\tst(r|1.)\tz[0-9]} } } */
/* { dg-final { scan-assembler {\tld(r|1.)\tz[0-9]} } } */
