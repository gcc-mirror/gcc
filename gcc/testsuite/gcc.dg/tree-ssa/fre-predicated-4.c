/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/117414 */

/* Fre1 should figure out that `*aaa != 0`
   For f0 and f1. */


void foo();
int f0(int *aaa, int j, int t)
{
  int b = *aaa;
  if (b == 0 || t == 1)
    return 0;
  for(int i = 0; i < j; i++)
  {
    if (!*aaa) foo();
  }
  return 0;
}

int f1(int *aaa, int j, int t)
{
  int b = *aaa;
  if (b == 0)
    return 0;
  if (t == 1)
    return 0;
  for(int i = 0; i < j; i++)
  {
    if (!*aaa) foo();
  }
  return 0;
}

/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
