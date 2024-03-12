/* { dg-do compile } */
/* { dg-options "-O2 -fsplit-paths -fdump-tree-split-paths-details " } */

double
foo(double *d1, double *d2, double *d3, int num, double *ip)
{
  double dmax[3];

  for (int i = 0; i < num; i++) {
    dmax[0] = d1[i] < dmax[0] ? dmax[0] : d1[i];
    dmax[1] = d2[i] < dmax[1] ? dmax[1] : d2[i];
    dmax[2] = d3[i] < dmax[2] ? dmax[2] : d3[i];
    ip[i] = dmax[2];
  }

  return dmax[0] + dmax[1] + dmax[2];
}

/* Split-paths shouldn't do anything here, if there's a diamond it would
   be if-convertible.  */
/* { dg-final { scan-tree-dump-not "Duplicating join block" "split-paths" } } */
