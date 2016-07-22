/* { dg-additional-options "-fdump-tree-cunroll-details" } */

#include "tree-vect.h"

int ii[32];
char cc[66] =
  { 0, 0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8, 0, 9, 0,
    10, 0, 11, 0, 12, 0, 13, 0, 14, 0, 15, 0, 16, 0, 17, 0, 18, 0, 19, 0,
    20, 0, 21, 0, 22, 0, 23, 0, 24, 0, 25, 0, 26, 0, 27, 0, 28, 0, 29, 0,
    30, 0, 31, 0 };

void __attribute__((noinline,noclone))
foo (int s)
{
  int i;
   for (i = 0; i < s; i++)
     ii[i] = (int) cc[i*2];
}

int main (int argc, const char **argv)
{
  int i;
  check_vect ();
  foo (32);
  for (i = 0; i < 32; i++)
    if (ii[i] != i)
      __builtin_abort ();
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump "loop turned into non-loop; it never loops" "cunroll" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump-not "loop with 2 iterations completely unrolled" "cunroll" { target { i?86-*-* x86_64-*-* } } } } */
