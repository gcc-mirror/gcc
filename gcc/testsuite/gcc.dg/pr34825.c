/* { dg-do compile } */
/* { dg-options "-O -funsafe-math-optimizations" } */

double foo(double x, double y)
{
  return x == y ? x/y*x/y : 0;
}

