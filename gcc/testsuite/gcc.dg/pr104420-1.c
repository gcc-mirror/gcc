/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-add-options ieee } */

double f(int a)
{
  return a * 0.0;
}

/* { dg-final { scan-tree-dump " \\\* 0.0" "optimized" } } */
