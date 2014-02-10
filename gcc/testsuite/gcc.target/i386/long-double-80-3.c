/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mlong-double-80 -mandroid" } */

long double
foo (long double x)
{
  return x * x;
}

/* { dg-final { scan-assembler "fldt" } } */
/* { dg-final { scan-assembler-not "call\[\\t \]*_?__multf3" } } */
