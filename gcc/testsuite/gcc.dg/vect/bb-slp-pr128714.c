/* { dg-additional-options "-fvect-cost-model=dynamic" } */

#include "tree-vect.h"

int m[2];
int r[2];

static inline void g (int *restrict a, int *p, int *q, int *o)
{
  *q = 5;                 /* q == p + 1: writes p[1] */
  int x0 = a[0];          /* restrict load; m[0] is never modified */
  int x1 = p[1];          /* must read 5 */
  o[0] = x0 * 3;
  o[1] = x1 * 3;
  *q = 7;
}

__attribute__((noipa)) void h (int *p, int *q, int *o) { g (p, p, q, o); }

int main ()
{
  check_vect ();
  m[0] = 1; m[1] = 2;
  h (m, m + 1, r);
  if (r[1] != 15)
    __builtin_abort ();
  return 0;
}
