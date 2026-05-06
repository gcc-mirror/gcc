/* Gather offsets that are not uniform per vector chunk: the start is not a
   multiple of the vector length (i starts at 1), the AND mask has low bits
   set, and the divisor is not a power of two.  The broadcast must not be
   used, and the result must be correct.  */

#include "tree-vect.h"

#define N 256

int table[N];
int out[N] __attribute__((aligned (64)));

void __attribute__((noipa))
f_base1 (int n)
{
  for (int i = 1; i < n; i++)
    out[i] = table[i >> 2];
}

void __attribute__((noipa))
f_and_odd (int n)
{
  for (int i = 0; i < n; i++)
    out[i] = table[i & 5];
}

void __attribute__((noipa))
f_div5 (int n)
{
  for (int i = 0; i < n; i++)
    out[i] = table[i / 5];
}

int
main (void)
{
  check_vect ();

  for (int i = 0; i < N; i++)
    table[i] = i * 7 + 3;

  f_base1 (N);
#pragma GCC novector
  for (int i = 1; i < N; i++)
    if (out[i] != table[i >> 2])
      __builtin_abort ();

  f_and_odd (N);
#pragma GCC novector
  for (int i = 0; i < N; i++)
    if (out[i] != table[i & 5])
      __builtin_abort ();

  f_div5 (N);
#pragma GCC novector
  for (int i = 0; i < N; i++)
    if (out[i] != table[i / 5])
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "computing cost for broadcast" "vect" } } */
