// { dg-do compile }

void foo(int i)
{
  int j;
  switch (i)			// { dg-error "invalid entry" }
  {
  #pragma omp parallel
    { case 0:; }
  #pragma omp for
    for (j = 0; j < 10; ++ j)
      { case 1:; }
  #pragma omp critical
    { case 2:; }
  #pragma omp master
    { case 3:; }
  #pragma omp sections
    { case 4:;
    #pragma omp section
       { case 5:; }
    }
  #pragma omp ordered
    { default:; }
  }
}
