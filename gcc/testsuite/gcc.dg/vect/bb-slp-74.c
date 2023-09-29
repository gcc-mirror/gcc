/* { dg-require-effective-target vect_double } */

#include "tree-vect.h"

double a[2], b[2], c[2];

void __attribute__((noipa)) foo ()
{
  double tem0 = a[1] + b[1];
  double tem1 = a[0] - b[0];
  c[0] = 2. * tem0;
  c[1] = 5. * tem1;
}

int main()
{
  check_vect ();

  a[0] = 1.; a[1] = 3.;
  b[0] = -5.; b[1] = 13.;
  foo ();
  if (c[0] != 32. || c[1] != 30.)
    __builtin_abort ();
  return 0;
}

/* We'd like to see at most one VEC_PERM_EXPR, not one for a blend
   and one for a permute materialized somewhere else.  But addsub
   pattern recog can likely get in the way here.  */
/* { dg-final { scan-tree-dump-times "  \[^ \]\+ = VEC_PERM_EXPR" 1 "slp2" } } */
