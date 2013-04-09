/* PR middle-end/56883 */
/* { dg-do compile }
/* { dg-options "-O2 -fopenmp" } */

void
f1 (int ***x)
{
  int i, j, k;
#pragma omp parallel for
  for (i = 0; i < 10; ++i)
    {
    #pragma omp parallel shared(j)
      #pragma omp for
	for (j = 0; j < 10; ++j)
	  {
	  #pragma omp parallel for
	      for (k = 0; k < 10; ++k)
		x[i][j][k] = k;
	  }
    }
}

void
f2 (int ***x)
{
  int i, j, k;
#pragma omp parallel for schedule(static,1)
  for (i = 0; i < 10; ++i)
    {
    #pragma omp parallel shared(j)
      #pragma omp for schedule(static,1)
	for (j = 0; j < 10; ++j)
	  {
	  #pragma omp parallel for schedule(static,1)
	      for (k = 0; k < 10; ++k)
		x[i][j][k] = k;
	  }
    }
}

void
f3 (int ***x)
{
  int i, j, k;
#pragma omp parallel for schedule(runtime)
  for (i = 0; i < 10; ++i)
    {
    #pragma omp parallel shared(j)
      #pragma omp for schedule(runtime)
	for (j = 0; j < 10; ++j)
	  {
	  #pragma omp parallel for schedule(runtime)
	      for (k = 0; k < 10; ++k)
		x[i][j][k] = k;
	  }
    }
}
