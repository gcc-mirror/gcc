/* { dg-add-options vect_early_break } */
/* { dg-additional-options "-std=gnu89" } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { ! "x86_64-*-* i?86-*-*" } } } } */

#include "tree-vect.h"

extern void abort ();
extern void exit (int);

__attribute__((noinline, noipa))
int f(x) {
  int i;
  for (i = 0; i < 8 && (x & 1) == 0; i++)
    x >>= 1;
  return i;
}
main() {
  check_vect ();

  if (f(4) != 2)
    abort();
  exit(0);
}
