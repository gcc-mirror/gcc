/* { dg-do compile } */

/* This test case is expected to fail due to errors.  */

#define N 30
#define M 3

int a[N][M], b[N][M], c[N][M];

extern void dostuff (int, int);

/* good1 and good2 should compile without error.  */
void
good1 (void)
{
  int x, shift;

  x = 0;
  #pragma omp parallel for simd collapse(2) reduction(inscan,+: x) private(shift)
  for (int i = 0; i < N; i++)
    {
      for (int j = 0; j < M; j++)
	{
	  x += a[i][j];
	  x += b[i][j];
#pragma omp scan inclusive(x)
	  shift = i + 29*j;
	  c[i][j] = x + shift;
	}
    }
}

void
good2 (void)
{
  int x, shift;
  x = 0;

  #pragma omp parallel for simd collapse(2) reduction(inscan,+: x) private(shift)
  for (int i = 0; i < N; i++)
    {
      for (int j = 0; j < M; j++)
	{
	  shift = i + 29*j;
	  c[i][j] = x + shift;
#pragma omp scan exclusive(x)
	  x += a[i][j];
	  x += b[i][j];
	}
    }
}

/* Adding intervening code should trigger an error.  */

void
bad1 (void)
{
  int x, shift;

  x = 0;
  #pragma omp parallel for simd collapse(2) reduction(inscan,+: x) private(shift)
  for (int i = 0; i < N; i++)  /* { dg-error "inner loops must be perfectly nested" } */
    {
      dostuff (i, 0);
      for (int j = 0; j < M; j++)
	{
	  x += a[i][j];
	  x += b[i][j];
#pragma omp scan inclusive(x)
	  shift = i + 29*j;
	  c[i][j] = x + shift;
	}
    }
}

void
bad2 (void)
{
  int x, shift;
  x = 0;

  #pragma omp parallel for simd collapse(2) reduction(inscan,+: x) private(shift)
  for (int i = 0; i < N; i++)  /* { dg-error "inner loops must be perfectly nested" } */
    {
      for (int j = 0; j < M; j++)
	{
	  shift = i + 29*j;
	  c[i][j] = x + shift;
#pragma omp scan exclusive(x)
	  x += a[i][j];
	  x += b[i][j];
	}
      dostuff (i, 1);
    }
}
