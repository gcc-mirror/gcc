/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mno-stv" } */

long long foo(long long x)
{
  return x << 1;
}

/* { dg-final { scan-assembler "adcl" } } */
/* { dg-final { scan-assembler-not "shldl" } } */
