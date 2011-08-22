/* { dg-do compile } */
/* { dg-options "-O3 -mavx -mno-avx2" } */

void
foo (int x, double *a, double *b, double c)
{
  int i;

  for (i = 0; i < x; i++)
    *a++ = *b++ * i / c;
}

/* { dg-final { scan-assembler-not "vpaddd\[ \\t\]+\[^\n\]*%ymm\[0-9\]" } } */
