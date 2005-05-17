/* { dg-do compile } */
/* { dg-options "-funsafe-math-optimizations -fdump-tree-gimple" } */

float f(float x)
{
  return x/2 + x/3;
}

float g(float x)
{
  return 2/x + 3/x;
}

float h(float x)
{
  return x/2 - x/3;
}

float i(float x)
{
  return 2/x - 3/x;
}

/* f and h should be turned into multiplications,
   the divisions in g and i should be grouped together.  */

/* { dg-final { scan-tree-dump-times " \\* " 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times " / " 2 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */

