/* PR optimization/5999
   This testcase ICEd because one a/b -> a * (1/b) optimization
   did not handle complex divides.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

__complex__ double foo (__complex__ double x, __complex__ double y)
{
  return x / y;
}
