/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mlong-double-80 -mbionic" } */

long double
foo (long double x)
{
  return x * x;
}

/* { dg-final { scan-assembler "fldt" } } */
