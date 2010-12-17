/* Verify that the 2 constant initializers are uniquized.  */

/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-gimple" } */

int lookup1 (int i)
{
  int a[] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  return a[i];
}

int lookup2 (int i)
{
  int a[] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  return a[i+1];
}

/* { dg-final { scan-tree-dump-times "L\\\$?C0" 2 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
