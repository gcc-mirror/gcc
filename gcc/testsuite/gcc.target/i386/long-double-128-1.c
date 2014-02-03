/* { dg-do compile } */
/* { dg-options "-O2 -mlong-double-128" } */

long double
foo (long double x)
{
  return x * x;
}

/* { dg-final { scan-assembler-not "fldt" } } */
/* { dg-final { scan-assembler "call\[\\t \]*_?__multf3" } } */
