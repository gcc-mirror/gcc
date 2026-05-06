/* Peeling for alignment by a known amount, forced with -mtune=atom.  The
   vector loop does not start at a multiple of the vector length, so the
   broadcast must not be used.  */
/* { dg-do run { target { x86_64-*-* i?86-*-* } } } */
/* { dg-additional-options "-mtune=atom -mprefer-vector-width=128" } */

#include "tree-vect.h"

#define N 2048

int table[N];
int out[N + 64] __attribute__((aligned (64)));

void __attribute__((noipa))
f (int n)
{
  for (int i = 0; i < n; i++)
    out[i + 1] = table[i >> 2];
}

int
main (void)
{
  check_vect ();

  volatile int n = 1013;
  for (int i = 0; i < N; i++)
    table[i] = i * 7 + 3;

  f (n);

#pragma GCC novector
  for (int i = 0; i < n; i++)
    if (out[i + 1] != table[i >> 2])
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "known peeling = 3" "vect" } } */
/* { dg-final { scan-tree-dump "not using broadcast for uniform gather offset because of peeling or epilogue" "vect" } } */
/* { dg-final { scan-tree-dump-not "computing cost for broadcast" "vect" } } */
