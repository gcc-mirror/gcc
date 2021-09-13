/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */

void
foo (long double x)
{
  asm("# %0" : "+fm"(x));
}
