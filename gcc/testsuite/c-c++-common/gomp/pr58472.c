/* PR tree-optimization/58472 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -fopenmp" } */

float a[1024], b[1024];

float
foo ()
{
  float s = 0.f;
  unsigned int i;
#pragma omp simd reduction(+:s)
  for (i = 0; i < 1024; ++i)
    s += a[i] * b[i];
  return s;
}
