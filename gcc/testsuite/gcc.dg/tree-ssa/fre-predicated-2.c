/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/117414 */

/* Fre1 should figure out that `*aaa != 0`
   For f0, f1, and f2. */


void foo();
int f0(int *aaa, int j, int t)
{
  int b = *aaa;
  int d = b | t;
  if (d == 0)
    ;
  else
    return 0;
  for(int i = 0; i < j; i++)
  {
    if (*aaa) foo();
  }
  return 0;
}

/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
