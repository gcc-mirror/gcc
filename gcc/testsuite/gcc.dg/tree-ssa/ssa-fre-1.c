/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre-details" } */

/* From PR27090.  */

int f(int *a)
{
  int t = *a;
  unsigned *b = (unsigned *)a;
  int *c = (int*)b;
  return *c + t;
}

/* { dg-final { scan-tree-dump "Replaced \\\(int \\\*\\\) b_.*with a_" "fre" } } */
/* { dg-final { scan-tree-dump "Replaced \\\*c_.*with t_" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
