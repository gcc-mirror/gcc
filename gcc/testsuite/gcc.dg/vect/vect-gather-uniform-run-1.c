/* Uniform gather offsets with no peeling: the vector loop starts at i = 0,
   so every chunk starts at a multiple of the vector length and the
   broadcast is valid.  Checks the result for each supported operator.  */

#include "tree-vect.h"

#define N 256

int table[N];
int out[N] __attribute__((aligned (64)));

void __attribute__((noipa))
f_shr (int n)
{
  for (int i = 0; i < n; i++)
    out[i] = table[i >> 2];
}

void __attribute__((noipa))
f_div (int n)
{
  for (int i = 0; i < n; i++)
    out[i] = table[i / 8];
}

void __attribute__((noipa))
f_and_not (int n)
{
  for (int i = 0; i < n; i++)
    out[i] = table[i & ~7];
}

void __attribute__((noipa))
f_and (int n)
{
  for (int i = 0; i < n; i++)
    out[i] = table[i & 8];
}

#define CHECK(EXPR)				\
  _Pragma ("GCC novector")			\
  for (int i = 0; i < N; i++)			\
    if (out[i] != table[EXPR])			\
      __builtin_abort ();

int
main (void)
{
  check_vect ();

  for (int i = 0; i < N; i++)
    table[i] = i * 7 + 3;

  f_shr (N);
  CHECK (i >> 2)
  f_div (N);
  CHECK (i / 8)
  f_and_not (N);
  CHECK (i & ~7)
  f_and (N);
  CHECK (i & 8)

  return 0;
}

/* { dg-final { scan-tree-dump "computing cost for broadcast" "vect" { target { x86_64-*-* i?86-*-* } } } } */
