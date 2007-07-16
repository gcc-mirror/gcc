/* { dg-do compile } */
/* { dg-options "-march=celledp -O1" } */
/* { dg-final { scan-assembler "dfcmeq" } } */

int foo(double x, double y)
{
  if (__builtin_fabs(x) == __builtin_fabs(y))
    return 0;
}
