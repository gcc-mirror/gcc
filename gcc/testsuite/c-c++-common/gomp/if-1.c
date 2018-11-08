/* { dg-do compile } */

void
foo (int a, int b, int *p, int *q)
{
  int i;
  #pragma omp parallel if (a)
    ;
  #pragma omp parallel if (parallel:a)
    ;
  #pragma omp parallel for simd if (a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp parallel for simd if (parallel : a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp parallel for simd if (simd : a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp parallel for simd if (simd : a) if (parallel:b)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp task if (a)
    ;
  #pragma omp task if (task: a)
    ;
  #pragma omp taskloop if (a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp taskloop if (taskloop : a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp taskloop simd if (a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp taskloop simd if (taskloop : a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp taskloop simd if (simd : a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp taskloop simd if (taskloop:b) if (simd : a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp target if (a)
    ;
  #pragma omp target if (target: a)
    ;
  #pragma omp target simd if (a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp target simd if (simd : a) if (target: b)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp target teams distribute parallel for simd if (a)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp target teams distribute parallel for simd if (parallel : a) if (target: b)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp target teams distribute parallel for simd if (simd : a) if (target: b)
  for (i = 0; i < 16; i++)
    ;
  #pragma omp target data if (a) map (p[0:2])
    ;
  #pragma omp target data if (target data: a) map (p[0:2])
    ;
  #pragma omp target enter data if (a) map (to: p[0:2])
  #pragma omp target enter data if (target enter data: a) map (to: p[0:2])
  #pragma omp target exit data if (a) map (from: p[0:2])
  #pragma omp target exit data if (target exit data: a) map (from: p[0:2])
  #pragma omp target update if (a) to (q[0:3])
  #pragma omp target update if (target update:a) to (q[0:3])
  #pragma omp parallel
  {
    #pragma omp cancel parallel if (a)
  }
 #pragma omp parallel
  {
    #pragma omp cancel parallel if (cancel:a)
  }
  #pragma omp for
  for (i = 0; i < 16; i++)
    {
      #pragma omp cancel for if (a)
    }
  #pragma omp for
  for (i = 0; i < 16; i++)
    {
      #pragma omp cancel for if (cancel: a)
    }
  #pragma omp sections
    {
    #pragma omp section
      {
	#pragma omp cancel sections if (a)
      }
    }
  #pragma omp sections
    {
    #pragma omp section
      {
	#pragma omp cancel sections if (cancel: a)
      }
    }
  #pragma omp taskgroup
  {
    #pragma omp task
    {
      #pragma omp cancel taskgroup if (a)
    }
    #pragma omp task
    {
      #pragma omp cancel taskgroup if (cancel: a)
    }
  }
}
