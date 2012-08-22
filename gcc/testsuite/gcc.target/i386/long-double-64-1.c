/* { dg-do compile } */
/* { dg-options "-O2 -mlong-double-64" } */

long double
foo (long double x)
{
  return x * x;
}

/* { dg-final { scan-assembler-not "fldt" } } */
