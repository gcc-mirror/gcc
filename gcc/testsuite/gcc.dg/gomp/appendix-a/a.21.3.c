/* { dg-do compile } */

void work (int);

void
a21_good (int n)
{
  int i;
#pragma omp for ordered
  for (i = 0; i < n; i++)
    {
      if (i <= 10)
	{
#pragma omp ordered
	  work (i);
	}
      if (i > 10)
	{
#pragma omp ordered
	  work (i + 1);
	}
    }
}
