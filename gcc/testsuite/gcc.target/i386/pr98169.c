/* PR tree-optimization/98169 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-finite-math-only" } */
/* { dg-final { scan-assembler-times "\tsetn\?p\t" 4 } } */
/* { dg-final { scan-assembler-not "\tjn\?\[ep]\t" } } */

int
f1 (float a)
{
  return a == a;
}

int
f2 (float a)
{
  return !__builtin_isnanf (a);
}

int
f3 (double a)
{
  return a == a;
}

int
f4 (double a)
{
  return !__builtin_isnan (a);
}
