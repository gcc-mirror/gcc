/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo(double x)
{
  return __builtin_isinf(x);
}

/* { dg-final { scan-assembler-times "testp.infinite.f64" 1 } } */
