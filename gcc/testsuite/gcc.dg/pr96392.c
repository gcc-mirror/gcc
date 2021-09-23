/* PR tree-optimization/96392 */
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

double plus0(int x)
{
  return x + 0.0;
}

double sub0(int x)
{
  return x - 0.0;
}

double mult0(int x)
{
  return 0.0 * x;
}

double negate(int x)
{
  return 0.0 - x;
}

double subtract(int x)
{
  return (double)x - (double)x;
}

/* { dg-final { scan-tree-dump-not " \\+ " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\- " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\* " "optimized" } } */

