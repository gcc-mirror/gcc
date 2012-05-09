/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ifcombine" } */

int test1 (int i, int j)
{
  if (i >= j)
    if (i != j)
      return 0;
  return -1;
}

/* The above should be optimized to a i > j test by ifcombine.  */

/* { dg-final { scan-tree-dump " > " "ifcombine" } } */
/* { dg-final { cleanup-tree-dump "ifcombine" } } */
