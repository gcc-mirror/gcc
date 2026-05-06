/* Peeling for alignment by an amount known only at run time: the store
   goes through a pointer whose alignment is unknown, so the peel count is
   computed at run time.  The vector loop's start is then unknown at compile
   time, so the broadcast must not be used.  Call with every misalignment.  */
/* { dg-do run { target { x86_64-*-* i?86-*-* } } } */
/* { dg-additional-options "-mtune=atom -mprefer-vector-width=128" } */

#include "tree-vect.h"

#define N 1024

int table[N];
int buf[N + 64] __attribute__((aligned (64)));

void __attribute__((noipa))
f (int *restrict out, int n)
{
  for (int i = 0; i < n; i++)
    out[i] = table[i >> 2];
}

int
main (void)
{
  check_vect ();

  for (int i = 0; i < N; i++)
    table[i] = i * 7 + 3;

  for (int off = 0; off < 4; off++)
    {
      int *out = buf + off;
      f (out, N - 8);
#pragma GCC novector
      for (int i = 0; i < N - 8; i++)
	if (out[i] != table[i >> 2])
	  __builtin_abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump "Peeling for alignment will be applied" "vect" } } */
/* { dg-final { scan-tree-dump-not "known peeling = " "vect" } } */
/* { dg-final { scan-tree-dump "not using broadcast for uniform gather offset because of peeling or epilogue" "vect" } } */
/* { dg-final { scan-tree-dump-not "computing cost for broadcast" "vect" } } */
