#pragma omp requires dynamic_allocators
void
foo ()
{
  int a = 10;
#pragma omp parallel private (a) allocate(a)
  a = 20;
#pragma omp target
  {
    #pragma omp parallel private (a) allocate(a)
    a = 30;
  }
#pragma omp target private(a) allocate(a)
  {
    a = 40;
  }
}

