/* PR middle-end/61486 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int
foo (int *a)
{
  int i, j = 0;
  #pragma omp target teams distribute simd linear(i, j) map(a[:10])
  for (i = 0; i < 10; i++)
    a[i] = j++;
  return i + j;
}
