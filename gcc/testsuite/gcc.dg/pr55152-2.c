/* { dg-do compile } */
/* { dg-options "-O -ffinite-math-only -fno-signed-zeros -fstrict-overflow -fdump-tree-optimized" } */

double g (double a)
{
  return (a<-a)?a:-a;
}
int f(int a)
{
  return (a<-a)?a:-a;
}

/* { dg-final { scan-tree-dump-times "ABS_EXPR" 2 "optimized" } } */
