/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-simd" } */

void
ne (double *zu)
{
  int h3;

#pragma omp simd simdlen (4)
  for (h3 = 0; h3 < 4; ++h3)
    zu[h3] = 0;
}
