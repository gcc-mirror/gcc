/* PR middle-end/121453 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -O2 -Wuninitialized" } */

void bar (int, int, int);
int v[40][40][40];

void
foo (int x, int y, int z)
{
  int i, j, k;
#pragma omp parallel for simd collapse(3)
  for (k = 1; k <= z; ++k)
    for (j = 2; j <= y - 1; ++j)
      for (i = 1; i <= x; ++i)
	v[i][j][k] = 0;
  bar (i, j, k);
}
