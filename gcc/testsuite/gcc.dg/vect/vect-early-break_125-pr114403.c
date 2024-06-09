/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_long_long } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"

long x[9];
long a[20];
struct { long x; long b[40]; } b;
int __attribute__((noipa))
foo (int n)
{
  int i = 0;
  int k = 0;
  do
    {
      if (x[k++])  // early exit, loop upper bound is 8 because of this
        break;
      a[i] = b.b[2*i]; // the misaligned 2*i access causes peeling for gaps
    }
  while (++i < n);
  return i;
}

int main()
{
  check_vect ();

  x[8] = 1;
  if (foo (20) != 8)
    __builtin_abort ();
  return 0;
}

