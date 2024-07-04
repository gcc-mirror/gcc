/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=zvl -ffast-math -fno-vect-cost-model" } */

#include <stdint-gcc.h>

void __attribute__((noipa))
add_loop (unsigned int *x, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < 0x7ff; ++i)
    {
      res0 += x[i * 2];
      res1 += x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

void __attribute__((noipa))
min_loop (unsigned int *x, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < 0x7ff; ++i)
    {
      res0 = res0 < x[i * 2] ? res0 : x[i * 2];
      res1 = res1 < x[i * 2 + 1] ? res1 : x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

void __attribute__((noipa))
max_loop (unsigned int *x, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < 0x7ff; ++i)
    {
      res0 = res0 > x[i * 2] ? res0 : x[i * 2];
      res1 = res1 > x[i * 2 + 1] ? res1 : x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

void __attribute__((noipa))
and_loop (unsigned int *x, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < 0x7ff; ++i)
    {
      res0 &= x[i * 2];
      res1 &= x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

void __attribute__((noipa))
or_loop (unsigned int *x, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < 0x7ff; ++i)
    {
      res0 |= x[i * 2];
      res1 |= x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}

void __attribute__((noipa))
eor_loop (unsigned int *x, unsigned int *res)
{
  unsigned int res0 = res[0];
  unsigned int res1 = res[1];
  for (int i = 0; i < 0x7ff; ++i)
    {
      res0 ^= x[i * 2];
      res1 ^= x[i * 2 + 1];
    }
  res[0] = res0;
  res[1] = res1;
}
