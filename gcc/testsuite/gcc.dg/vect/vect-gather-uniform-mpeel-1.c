/* Peeling with a mask.  With AVX-512 and partial vectors the vectorizer
   aligns the store by starting the first vector iteration before i = 0 and
   masking off the leading lanes, so lane 0 is inactive and every chunk is
   shifted.  Masked peeling implies LOOP_VINFO_PEELING_FOR_ALIGNMENT != 0,
   so the broadcast is rejected.  An emulated gather cannot be masked, so
   this loop uses a real gather, which the broadcast does not handle yet.
   The test guards against extending the broadcast to real gathers without
   rejecting masked peeling.  */
/* { dg-do run { target { x86_64-*-* i?86-*-* } } } */
/* { dg-require-effective-target avx512f_runtime } */
/* { dg-additional-options "-mavx512f -mprefer-vector-width=512 -mtune=atom --param vect-partial-vector-usage=2" } */

#include "tree-vect.h"

#define N 2048

int table[N];
int out[N + 64] __attribute__((aligned (64)));

void __attribute__((noipa))
f (int n)
{
  for (int i = 0; i < n; i++)
    out[i + 1] = table[i >> 4];
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
    if (out[i + 1] != table[i >> 4])
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "misalignment for fully-masked loop" "vect" } } */
/* { dg-final { scan-tree-dump-not "computing cost for broadcast" "vect" } } */
