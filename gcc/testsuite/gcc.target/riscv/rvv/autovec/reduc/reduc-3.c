/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -ffast-math -fno-vect-cost-model" } */

#include <stdint-gcc.h>

unsigned short __attribute__((noipa))
add_loop (unsigned short *x, int n)
{
  unsigned short res = 0;
  for (int i = 0; i < n; ++i)
    res += x[i];
  return res;
}

unsigned short __attribute__((noipa))
min_loop (unsigned short *x, int n)
{
  unsigned short res = ~0;
  for (int i = 0; i < n; ++i)
    res = res < x[i] ? res : x[i];
  return res;
}

unsigned short __attribute__((noipa))
max_loop (unsigned short *x, int n)
{
  unsigned short res = 0;
  for (int i = 0; i < n; ++i)
    res = res > x[i] ? res : x[i];
  return res;
}

unsigned short __attribute__((noipa))
and_loop (unsigned short *x, int n)
{
  unsigned short res = ~0;
  for (int i = 0; i < n; ++i)
    res &= x[i];
  return res;
}

unsigned short __attribute__((noipa))
or_loop (unsigned short *x, int n)
{
  unsigned short res = 0;
  for (int i = 0; i < n; ++i)
    res |= x[i];
  return res;
}

unsigned short __attribute__((noipa))
eor_loop (unsigned short *x, int n)
{
  unsigned short res = 0;
  for (int i = 0; i < n; ++i)
    res ^= x[i];
  return res;
}

/* { dg-final { scan-assembler-times {vredsum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vredmaxu\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vredminu\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vredand\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vredor\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vredxor\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
