/* PR middle-end/78884 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp" } */

void bar (int *);

void
foo (int n)
{
#pragma omp simd
  for (int i = 0; i < 1024; i++)
    {
      int vla[n];
      bar (vla);
    }
}
