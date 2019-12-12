/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (void)
{
  int i, j;
  #pragma omp for ordered (1)
  for (i = 0; i < 64; i++)
    {
      #pragma omp ordered depend (sink: i + 1)	/* { dg-warning "'depend' clause with 'sink' modifier waiting for lexically later iteration" } */
      #pragma omp ordered depend (source)
    }
  #pragma omp for ordered (1)
  for (i = 63; i >= 0; i--)
    {
      #pragma omp ordered depend (sink: i - 1)	/* { dg-warning "'depend' clause with 'sink' modifier waiting for lexically later iteration" } */
      #pragma omp ordered depend (source)
    }
  #pragma omp for ordered (2) collapse (2)
  for (i = 0; i < 64; i++)
    for (j = 0; j < 64; j++)
      {
	#pragma omp ordered depend (sink: i + 1, j - 2)	/* { dg-warning "'depend' clause with 'sink' modifier waiting for lexically later iteration" } */
	#pragma omp ordered depend (source)
      }
  #pragma omp for ordered (2) collapse (2)
  for (i = 63; i >= 0; --i)
    for (j = 0; j < 64; j++)
      {
	#pragma omp ordered depend (sink: i - 2, j - 2)	/* { dg-warning "'depend' clause with 'sink' modifier waiting for lexically later iteration" } */
	#pragma omp ordered depend (source)
      }
  #pragma omp for ordered (2) collapse (2)
  for (i = 0; i < 64; i++)
    for (j = 0; j < 64; j++)
      {
	#pragma omp ordered depend (sink: i - 1, j + 2)
	#pragma omp ordered depend (source)
      }
  #pragma omp for ordered (2) collapse (2)
  for (i = 63; i >= 0; --i)
    for (j = 0; j < 64; j++)
      {
	#pragma omp ordered depend (sink: i + 2, j + 2)
	#pragma omp ordered depend (source)
      }
  #pragma omp for ordered (1)
  for (i = 0; i < 64; i += 2)
    {
      #pragma omp ordered depend (sink: i - 1)	/* { dg-warning "'depend' clause with 'sink' modifier refers to iteration never in the iteration space" } */
      #pragma omp ordered depend (source)
    }
}
