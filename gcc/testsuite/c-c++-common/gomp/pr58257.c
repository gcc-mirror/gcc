/* PR middle-end/58257 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -Wall" } */

int
foo (int n)
{
  int a[10][10];
  int x, y;
#pragma omp parallel for collapse(2)	/* { dg-bogus "may be used uninitialized in this function" } */
  for (x = 0; x < n; x++)		/* { dg-bogus "may be used uninitialized in this function" } */
    for (y = 0; y < n; y++)
      a[x][y] = x + y * y;
  return a[0][0];
}
