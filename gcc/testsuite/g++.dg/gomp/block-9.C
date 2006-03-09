// { dg-do compile }

void foo(int i)
{
  int j;
  switch (i)
  {
  #pragma omp parallel
    { case 0:; }		// { dg-error "jump|enters" }
  #pragma omp for
    for (j = 0; j < 10; ++ j)
      { case 1:; }		// { dg-error "jump|enters" }
  #pragma omp critical
    { case 2:; }		// { dg-error "jump|enters" }
  #pragma omp master
    { case 3:; }		// { dg-error "jump|enters" }
  #pragma omp sections
    { case 4:;			// { dg-error "jump|enters" }
    #pragma omp section
       { case 5:; }		// { dg-error "jump|enters" }
    }
  #pragma omp ordered
    { default:; }		// { dg-error "jump|enters" }
  }
}
