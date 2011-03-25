/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

/* From PR27090.  */

int f(int *a)
{
  int t = *a;
  unsigned *b = (unsigned *)a;
  int *c = (int*)b;
  return *c + t;
}

/* { dg-final { scan-tree-dump "Replaced \\\*c_\[^\n\].*with t_" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
