/* PR tree-optimization/92557 */
/* { dg-do compile } */
/* { dg-additional-options "-maltivec" { target powerpc*-*-* } } */

void
foo (double *p)
{
  int i;

#pragma omp simd aligned (p)
  for (i = 0; i < 1; ++i)
    p[i] = 7.0;
}
