/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake-avx512 -mprefer-avx256" } */
/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */

float
my_test_f()
{
  return 0.0f;
}

double
my_test_d()
{
  return 0.0;
}
