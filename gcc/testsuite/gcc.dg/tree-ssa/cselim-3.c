/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1-details-stats" } */
/* PR tree-optimization/122155 */

void f(int *a, int b, int c)
{
  if (c)
    *a = b;
  else
    *a = b;
}

/* When commonalizing a store with the same rhs, a PHI does not need to be created. */
/* { dg-final { scan-tree-dump "if-then-else store replacement: 1" "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "to use phi:" "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "PHI <" "phiopt1" } } */
