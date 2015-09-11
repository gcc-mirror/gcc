/* PR c/67502 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */
/* { dg-additional-options "-std=c99" { target c } } */

void bar (int, int);

void
foo (void)
{
#pragma omp parallel
#pragma omp for simd collapse(2)
  for (int i = 0; i < 16; ++i)
    for (int j = 0; j < 16; ++j)
      bar (i, j);
}
