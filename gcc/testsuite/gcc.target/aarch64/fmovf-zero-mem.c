/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (float *output)
{
  *output = 0.0;
}

/* { dg-final { scan-assembler "str\\twzr, \\\[x0\\\]" } } */
