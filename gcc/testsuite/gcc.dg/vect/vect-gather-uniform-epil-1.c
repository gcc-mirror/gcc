/* Epilogue vectorization.  The main loop uses 8 lanes and peels 7
   iterations for alignment; i >> 2 is not uniform over 8 lanes, so the main
   loop uses the full emulated gather.  The 4-lane epilogue starts at
   i = 7 + 8 * k, which is not a multiple of 4, so the broadcast must not be
   used there either.  The tune-ctrl options keep the gather emulated and
   -fno-vect-cost-model keeps the main loop at 8 lanes.  */
/* { dg-do run { target { x86_64-*-* i?86-*-* } } } */
/* { dg-require-effective-target avx2_runtime } */
/* { dg-additional-options "-mavx2 -mtune=atom -mtune-ctrl=^use_gather_2parts,^use_gather_4parts,^use_gather_8parts" } */

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

/* { dg-final { scan-tree-dump "epilogue loop vectorized using 16 byte vectors" "vect" } } */
/* { dg-final { scan-tree-dump "not using broadcast for uniform gather offset because of peeling or epilogue" "vect" } } */
/* { dg-final { scan-tree-dump-not "computing cost for broadcast" "vect" } } */
