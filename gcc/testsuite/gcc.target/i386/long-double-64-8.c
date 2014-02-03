/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mlong-double-64 -mbionic" } */

long double
foo (long double x)
{
  return x * x;
}

/* { dg-final { scan-assembler-not "fldt" } } */
/* { dg-final { scan-assembler-not "call\[\\t \]*_?__multf3" } } */
