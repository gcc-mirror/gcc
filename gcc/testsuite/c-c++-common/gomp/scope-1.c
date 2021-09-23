int r, r2, r3;

void
foo (void)
{
  int i = 0, j = 0, k = 0;
  #pragma omp scope private (i) reduction (+:r) nowait
  {
    i = 1;
    r++;
  }
  #pragma omp scope private (i) reduction (task, +:r)
  #pragma omp scope private (j) reduction (task, +:r2)
  #pragma omp scope private (k) reduction (task, +:r3)
  {
    i = 1;
    j = 2;
    k = 3;
    r++;
    r2++;
    r3++;
  }
  #pragma omp parallel
  {
    #pragma omp scope reduction (+:r) private (i) nowait
    {
      #pragma omp scope reduction (+:r2) private (j) nowait
      {
	#pragma omp single
	{
	  i = 1;
	  j = 2;
	  r++;
	  r2++;
	}
      }
    }
  }
}
