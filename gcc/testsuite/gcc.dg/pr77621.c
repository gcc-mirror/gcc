/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-mtune=atom -msse2" { target i?86-*-* x86_64-*-* } } */

void
foo (double *x, int *y)
{
  int i;
  for (i = 0; i < 8; i++)
    x[i] -= y[i] * x[i + 1];
}
