void bar (void);

void
foo (int x, unsigned long long y)
{
  #pragma omp task
    bar ();
  #pragma omp taskloop
    for (int i = 0; i < 10; i++)
      bar ();
  #pragma omp task
    bar ();
  #pragma omp taskloop
    for (unsigned long long int i = 0; i < y; i++)
      bar ();
  #pragma omp task priority (1)
    bar ();
  #pragma omp taskloop priority (1)
    for (int i = 0; i < 10; i++)
      bar ();
  #pragma omp task priority (x + 1)
    bar ();
  #pragma omp taskloop priority (x + 1)
    for (unsigned long long int i = 0; i < y; i++)
      bar ();
}
