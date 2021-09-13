/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */

long double
foo (long double x)
{
  x = x * x;
  asm("# %0" : "+fvm"(x));
  x = x + x;
  return x;
}
