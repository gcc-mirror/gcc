/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-add-options ieee } */

double f(unsigned int a)
{
  return a * 0.0;
}

/* { dg-final { scan-tree-dump "return 0.0" "optimized" } } */
