/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int *f(int *b)
{
  int * a = new int[104];
  *a = 1;
  if (a == 0)
    return b;
  return a;
}

/* { dg-final { scan-tree-dump-not "if" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
