/* Uniform gather offset with a non-zero start that is a multiple of the
   vector length (i starts at 8).  Every chunk still starts at a multiple
   of the vector length, so the broadcast is valid.  */

#include "tree-vect.h"

#define N 256

int table[N];
int out[N] __attribute__((aligned (64)));

void __attribute__((noipa))
f (int n)
{
  for (int i = 8; i < n; i++)
    out[i] = table[i >> 2];
}

int
main (void)
{
  check_vect ();

  for (int i = 0; i < N; i++)
    table[i] = i * 7 + 3;

  f (N);

#pragma GCC novector
  for (int i = 8; i < N; i++)
    if (out[i] != table[i >> 2])
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "computing cost for broadcast" "vect" { target { x86_64-*-* i?86-*-* } } } } */
