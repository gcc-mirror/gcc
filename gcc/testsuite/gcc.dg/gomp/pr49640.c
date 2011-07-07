/* PR middle-end/49640 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu99 -fopenmp" } */

void
foo (int N, int M, int K, int P, int Q, int R, int i, int j, int k,
     unsigned char x[P][Q][R], int y[N][M][K])
{
  int ii, jj, kk;

#pragma omp parallel for private(ii,jj,kk)
  for (ii = 0; ii < P; ++ii)
    for (jj = 0; jj < Q; ++jj)
      for (kk = 0; kk < R; ++kk)
	y[i + ii][j + jj][k + kk] = x[ii][jj][kk];
}

void
bar (int N, int M, int K, int P, int Q, int R, int i, int j, int k,
     unsigned char x[P][Q][R], float y[N][M][K], float factor, float zero)
{
  int ii, jj, kk;

#pragma omp parallel for private(ii,jj,kk)
  for (ii = 0; ii < P; ++ii)
    for (jj = 0; jj < Q; ++jj)
      for (kk = 0; kk < R; ++kk)
	y[i + ii][j + jj][k + kk] = factor * x[ii][jj][kk] + zero;
}
