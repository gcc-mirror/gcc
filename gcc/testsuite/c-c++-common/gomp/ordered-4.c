void
f1 (void)
{
  int i, j;
  #pragma omp critical
  {
    #pragma omp simd
    for (i = 0; i < 64; i++)
      {
	#pragma omp ordered simd
	;
      }
  }
  #pragma omp ordered threads
  {
    #pragma omp simd
    for (i = 0; i < 64; i++)
      {
	#pragma omp ordered simd
	;
      }
  }
  #pragma omp task
  {
    #pragma omp simd
    for (i = 0; i < 64; i++)
      {
	#pragma omp ordered simd
	;
      }
  }
  #pragma omp taskloop
  for (j = 0; j < 64; j++)
    #pragma omp simd
    for (i = 0; i < 64; i++)
      {
	#pragma omp ordered simd
	;
      }
}

void
f2 (void)
{
  #pragma omp ordered simd
  ;
}

void
f3 (void)
{
  #pragma omp ordered threads , simd
  ;
}
