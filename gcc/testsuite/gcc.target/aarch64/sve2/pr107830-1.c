/* { dg-do compile } */
/* { dg-require-effective-target fopenmp } */
/* { dg-additional-options "-Os -fopenmp" } */

void
f2 (int *a)
{
  unsigned int i;

#pragma omp simd
  for (i = 0; i < 4; ++i)
    a[i / 3] -= 4;
}
