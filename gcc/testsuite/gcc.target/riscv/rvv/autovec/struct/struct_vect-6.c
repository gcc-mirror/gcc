/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#ifndef TYPE
#define TYPE uint8_t
#define ITYPE int8_t
#endif

void __attribute__ ((noinline, noclone))
f2 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      a[i] = c[i * 2];
      b[i] = c[i * 2 + 1];
    }
}

void __attribute__ ((noinline, noclone))
f3 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      a[i] = d[i * 3];
      b[i] = d[i * 3 + 1];
      c[i] = d[i * 3 + 2];
    }
}

void __attribute__ ((noinline, noclone))
f4 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, TYPE *__restrict e, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      a[i] = e[i * 4];
      b[i] = e[i * 4 + 1];
      c[i] = e[i * 4 + 2];
      d[i] = e[i * 4 + 3];
    }
}

void __attribute__ ((noinline, noclone))
f5 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, TYPE *__restrict e, TYPE *__restrict f, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      a[i] = f[i * 5];
      b[i] = f[i * 5 + 1];
      c[i] = f[i * 5 + 2];
      d[i] = f[i * 5 + 3];
      e[i] = f[i * 5 + 4];
    }
}

void __attribute__ ((noinline, noclone))
f6 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, TYPE *__restrict e, TYPE *__restrict f,
    TYPE *__restrict g, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      a[i] = g[i * 6];
      b[i] = g[i * 6 + 1];
      c[i] = g[i * 6 + 2];
      d[i] = g[i * 6 + 3];
      e[i] = g[i * 6 + 4];
      f[i] = g[i * 6 + 5];
    }
}

void __attribute__ ((noinline, noclone))
f7 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, TYPE *__restrict e, TYPE *__restrict f,
    TYPE *__restrict g, TYPE *__restrict h, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      a[i] = h[i * 7];
      b[i] = h[i * 7 + 1];
      c[i] = h[i * 7 + 2];
      d[i] = h[i * 7 + 3];
      e[i] = h[i * 7 + 4];
      f[i] = h[i * 7 + 5];
      g[i] = h[i * 7 + 6];
    }
}

void __attribute__ ((noinline, noclone))
f8 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, TYPE *__restrict e, TYPE *__restrict f,
    TYPE *__restrict g, TYPE *__restrict h, TYPE *__restrict j, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      a[i] = j[i * 8];
      b[i] = j[i * 8 + 1];
      c[i] = j[i * 8 + 2];
      d[i] = j[i * 8 + 3];
      e[i] = j[i * 8 + 4];
      f[i] = j[i * 8 + 5];
      g[i] = j[i * 8 + 6];
      h[i] = j[i * 8 + 7];
    }
}

void __attribute__ ((noinline, noclone))
g2 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      c[i * 2] = a[i];
      c[i * 2 + 1] = b[i];
    }
}

void __attribute__ ((noinline, noclone))
g3 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      d[i * 3] = a[i];
      d[i * 3 + 1] = b[i];
      d[i * 3 + 2] = c[i];
    }
}

void __attribute__ ((noinline, noclone))
g4 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, TYPE *__restrict e, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      e[i * 4] = a[i];
      e[i * 4 + 1] = b[i];
      e[i * 4 + 2] = c[i];
      e[i * 4 + 3] = d[i];
    }
}

void __attribute__ ((noinline, noclone))
g5 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, TYPE *__restrict e, TYPE *__restrict f, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      f[i * 5] = a[i];
      f[i * 5 + 1] = b[i];
      f[i * 5 + 2] = c[i];
      f[i * 5 + 3] = d[i];
      f[i * 5 + 4] = e[i];
    }
}

void __attribute__ ((noinline, noclone))
g6 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, TYPE *__restrict e, TYPE *__restrict f,
    TYPE *__restrict g, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      g[i * 6] = a[i];
      g[i * 6 + 1] = b[i];
      g[i * 6 + 2] = c[i];
      g[i * 6 + 3] = d[i];
      g[i * 6 + 4] = e[i];
      g[i * 6 + 5] = f[i];
    }
}

void __attribute__ ((noinline, noclone))
g7 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, TYPE *__restrict e, TYPE *__restrict f,
    TYPE *__restrict g, TYPE *__restrict h, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      h[i * 7] = a[i];
      h[i * 7 + 1] = b[i];
      h[i * 7 + 2] = c[i];
      h[i * 7 + 3] = d[i];
      h[i * 7 + 4] = e[i];
      h[i * 7 + 5] = f[i];
      h[i * 7 + 6] = g[i];
    }
}

void __attribute__ ((noinline, noclone))
g8 (TYPE *__restrict a, TYPE *__restrict b, TYPE *__restrict c,
    TYPE *__restrict d, TYPE *__restrict e, TYPE *__restrict f,
    TYPE *__restrict g, TYPE *__restrict h, TYPE *__restrict j, ITYPE n)
{
  for (ITYPE i = 0; i < n; ++i)
    {
      j[i * 8] = a[i];
      j[i * 8 + 1] = b[i];
      j[i * 8 + 2] = c[i];
      j[i * 8 + 3] = d[i];
      j[i * 8 + 4] = e[i];
      j[i * 8 + 5] = f[i];
      j[i * 8 + 6] = g[i];
      j[i * 8 + 7] = h[i];
    }
}

/* { dg-final { scan-assembler-times {vlseg2e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg3e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg4e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg5e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg6e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg7e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vlseg8e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg2e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg3e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg4e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg5e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg6e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg7e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsseg8e8\.v} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]} 14 } } */
