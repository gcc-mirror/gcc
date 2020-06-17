/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#include <stdint.h>

#ifndef TYPE
#define TYPE uint8_t
#endif

#ifndef NAME
#define NAME(X) X
#endif

#ifndef N
#define N 1024
#endif

void __attribute__ ((noinline, noclone))
NAME(f2) (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c)
{
  for (int i = 0; i < N; ++i)
    {
      a[i] = c[i * 2];
      b[i] = c[i * 2 + 1];
    }
}

void __attribute__ ((noinline, noclone))
NAME(f3) (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
	  TYPE *__restrict d)
{
  for (int i = 0; i < N; ++i)
    {
      a[i] = d[i * 3];
      b[i] = d[i * 3 + 1];
      c[i] = d[i * 3 + 2];
    }
}

void __attribute__ ((noinline, noclone))
NAME(f4) (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
	  TYPE *__restrict d, TYPE *__restrict e)
{
  for (int i = 0; i < N; ++i)
    {
      a[i] = e[i * 4];
      b[i] = e[i * 4 + 1];
      c[i] = e[i * 4 + 2];
      d[i] = e[i * 4 + 3];
    }
}

void __attribute__ ((noinline, noclone))
NAME(g2) (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c)
{
  for (int i = 0; i < N; ++i)
    {
      c[i * 2] = a[i];
      c[i * 2 + 1] = b[i];
    }
}

void __attribute__ ((noinline, noclone))
NAME(g3) (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
	  TYPE *__restrict d)
{
  for (int i = 0; i < N; ++i)
    {
      d[i * 3] = a[i];
      d[i * 3 + 1] = b[i];
      d[i * 3 + 2] = c[i];
    }
}

void __attribute__ ((noinline, noclone))
NAME(g4) (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
	  TYPE *__restrict d, TYPE *__restrict e)
{
  for (int i = 0; i < N; ++i)
    {
      e[i * 4] = a[i];
      e[i * 4 + 1] = b[i];
      e[i * 4 + 2] = c[i];
      e[i * 4 + 3] = d[i];
    }
}

/* { dg-final { scan-assembler {\tld2b\t{z[0-9]+.b - z[0-9]+.b}, p[0-7]/z, \[x[0-9]+, x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tld3b\t{z[0-9]+.b - z[0-9]+.b}, p[0-7]/z, \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tld4b\t{z[0-9]+.b - z[0-9]+.b}, p[0-7]/z, \[x[0-9]+, x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tst2b\t{z[0-9]+.b - z[0-9]+.b}, p[0-7], \[x[0-9]+, x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tst3b\t{z[0-9]+.b - z[0-9]+.b}, p[0-7], \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tst4b\t{z[0-9]+.b - z[0-9]+.b}, p[0-7], \[x[0-9]+, x[0-9]+\]\n} } } */
