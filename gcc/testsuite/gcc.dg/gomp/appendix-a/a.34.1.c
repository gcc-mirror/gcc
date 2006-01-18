/* { dg-do compile } */

void
work (int i, int j)
{
}

void
good_nesting (int n)
{
  int i, j;
#pragma omp parallel default(shared)
  {
#pragma omp for
    for (i = 0; i < n; i++)
      {
#pragma omp parallel shared(i, n)
	{
#pragma omp for
	  for (j = 0; j < n; j++)
	    work (i, j);
	}
      }
  }
}
