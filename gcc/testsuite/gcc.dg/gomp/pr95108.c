/* PR middle-end/95108 */
/* { dg-do compile { target vect_simd_clones } } */
/* { dg-options "-O2 -fopenmp-simd -w" } */

int *v;

#pragma omp declare simd
void
foo (int x)
{
  int *a = &x + 1;

  for (;;)
    {
      *v = *a;
      a = v;
    }
}
