/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a" } */

#include <stdint.h>
#include <stdio.h>

#pragma GCC target "+nosve"

int __attribute__((noinline))
f(uint64_t *__restrict a, uint64_t *__restrict b, uint64_t *__restrict c)
{
  int i;

  for (i = 0; i < 16; i++)
    a[i] = b[i] >> c[i];
}


int __attribute__((noinline))
g(int64_t *__restrict a, int64_t *__restrict b, int64_t *__restrict c)
{
  int i;

  for (i = 0; i < 16; i++)
    a[i] = b[i] >> c[i];
}

/* { dg-final { scan-assembler "neg\\tv" } } */
/* { dg-final { scan-assembler "ushl\\tv" } } */
/* { dg-final { scan-assembler "sshl\\tv" } } */
