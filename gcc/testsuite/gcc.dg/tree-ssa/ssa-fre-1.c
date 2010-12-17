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

/* { dg-final { scan-tree-dump "Replaced \\\*a_\[^\n\].*with t_" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
