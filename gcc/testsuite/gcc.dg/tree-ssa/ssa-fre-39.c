/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

int foo (int i)
{
  int k = i + 1;
  int j = i + 1;
  if (k != j)
    k = k + 1;
  if (k != j)
    k = k + 1;
  k = k - i;
  return k;
}

/* We should be able to value-number the final assignment to k to 1.  */

/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */
