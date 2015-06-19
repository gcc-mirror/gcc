/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (long double *output)
{
  *output = 0.0;
}

/* { dg-final { scan-assembler "stp\\txzr, xzr, \\\[x0\\\]" } } */
