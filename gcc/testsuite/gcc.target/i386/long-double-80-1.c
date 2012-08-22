/* { dg-do compile } */
/* { dg-options "-O2 -mlong-double-80" } */

long double
foo (long double x)
{
  return x * x;
}

/* { dg-final { scan-assembler "fldt" } } */
