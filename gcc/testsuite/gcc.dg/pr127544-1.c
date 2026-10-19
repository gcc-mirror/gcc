/* { dg-do compile } */
/* { dg-options "-O2 -fno-math-errno -fno-signed-zeros -fdump-tree-optimized" } */

double foo()
{
  return __builtin_pow(-__builtin_inf(),0.5);
}

double bar(double x)
{
  return __builtin_pow(x,0.5);
}

/* { dg-final { scan-tree-dump-not "sqrt" "optimized" } } */
