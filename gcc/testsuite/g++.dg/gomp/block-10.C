// { dg-do compile }

void foo(int i)
{
  int j;
  switch (i)
  {
  #pragma omp parallel	// { dg-warning "statement will never be executed" }
    { case 0:; }	// { dg-error "jump|enters" }
  }
  switch (i)
  {
  #pragma omp for	// { dg-warning "statement will never be executed" }
    for (j = 0; j < 10; ++ j)
      { case 1:; }	// { dg-error "jump|enters" }
  }
  switch (i)
  {
  #pragma omp critical	// { dg-warning "statement will never be executed" }
    { case 2:; }	// { dg-error "jump|enters" }
  }
  switch (i)
  {
  #pragma omp master	// { dg-warning "statement will never be executed" }
    { case 3:; }	// { dg-error "jump|enters" }
  }
  switch (i)
  {
  #pragma omp sections	// { dg-warning "statement will never be executed" }
    { case 4:;		// { dg-error "jump|enters" }
    #pragma omp section
       { case 5:; }	// { dg-error "jump|enters" }
    }
  }
  switch (i)
  {
  #pragma omp ordered	// { dg-warning "statement will never be executed" }
    { default:; }	// { dg-error "jump|enters" }
  }
}
