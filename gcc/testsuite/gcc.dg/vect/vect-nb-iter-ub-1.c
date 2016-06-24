/* { dg-additional-options "-fdump-tree-cunroll-details" } */

#include "tree-vect.h"

int ii[31];
char cc[31] =
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 };

void __attribute__((noinline,noclone))
foo (int s)
{
  int i;
  for (i = 0; i < s; i++)
    ii[i] = (int) cc[i];
}

int main (int argc, const char **argv)
{
  int i;
  check_vect ();
  foo (31);
  for (i = 0; i < 31; i++)
    if (ii[i] != i)
      __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump "loop turned into non-loop; it never loops" "cunroll" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump-not "loop with 2 iterations completely unrolled" "cunroll" { target { i?86-*-* x86_64-*-* } } } } */
