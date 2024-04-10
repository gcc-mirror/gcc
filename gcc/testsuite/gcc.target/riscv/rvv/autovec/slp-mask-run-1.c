/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=gnu99 -O3 -mrvv-vector-bits=scalable" } */

#include <malloc.h>
#include <stdio.h>

#include "slp-mask-1.c"

#define SZ 8

__attribute__ ((optimize ("1")))
int main ()
{
  int *a = malloc (SZ * sizeof (*a));
  short *b = malloc (SZ * sizeof (*b));
  int *res = malloc (SZ * sizeof (*res));
  int *ref = malloc (SZ * sizeof (*ref));

  for (int i = 0; i < SZ; i++)
    {
      a[i] = i & 1;
      b[i] = 2;
      ref[i] = a[i] == 1 & b[i] == 2;
    }

  f (a, b, res);

  for (int i = 0; i < SZ; i++)
    if (res[i] != ref[i])
      __builtin_abort ();
}
