int r, r2, r3 = 1;
int bar (void);

void
foo (void)
{
  int i = 0, j = 0, k = 0;
  #pragma omp parallel
  {
    if (bar ())
      {
	#pragma omp cancel parallel
      }
    #pragma omp scope reduction (+:r) private (i)
    {
      #pragma omp scope reduction (+:r2) private (j)
      {
	#pragma omp single nowait
	{
	  i = 1;
	  j = 2;
	  r++;
	  r2++;
	}
      }
    }
  }
  #pragma omp parallel
  {
    if (bar ())
      {
	#pragma omp cancel parallel
      }
    #pragma omp scope reduction (task, +:r) private (i)
    #pragma omp scope reduction (task, *:r3)
    {
      r++;
      r3++;
    }
  }
}
