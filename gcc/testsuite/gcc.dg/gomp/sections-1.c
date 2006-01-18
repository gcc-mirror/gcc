/* { dg-do compile } */

extern void bar(int);

void f1(void)
{
  #pragma omp sections nowait
    {
      bar (1);
    #pragma omp section
      bar (2);
    #pragma omp section
      bar (3);
    #pragma omp section
      bar (4);
    #pragma omp section
      bar (5);
    }
}

void f2(void)
{
  #pragma omp sections
    {
    #pragma omp section
      {
        bar (1);
        bar (1);
      }
    #pragma omp section
      bar (2);
    #pragma omp section
      bar (3);
    #pragma omp section
      bar (4);
    #pragma omp section
      bar (5);
    }
}
