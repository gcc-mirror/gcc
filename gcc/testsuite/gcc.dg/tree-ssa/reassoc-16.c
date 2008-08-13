/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-reassoc1" } */

double test1 (double x, double y, double z, double weight)
{
  double tmp1 = x / weight;
  double tmp2 = y / weight;
  double tmp3 = -x / weight;
  return tmp1 + tmp2 + tmp3;
}

/* The division should be un-distributed and all references to x should
   be gone.  */

/* { dg-final { scan-tree-dump-times "/" 1 "reassoc1" } } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
