/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -mandroid" } */

long double
foo (long double x)
{
  return x * x;
}

/* { dg-final { scan-assembler-not "fldt" } } */
/* { dg-final { scan-assembler "call\[\\t \]*_?__multf3" } } */
