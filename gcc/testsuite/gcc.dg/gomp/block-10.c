// { dg-do compile }

void foo(int i)
{
  int j;
  switch (i) // { dg-error "invalid entry to OpenMP structured block" }
  {
  #pragma omp parallel // { dg-warning "statement will never be executed" }
    { case 0:; }
  }
  switch (i) // { dg-error "invalid entry to OpenMP structured block" }
  {
  #pragma omp for // { dg-warning "statement will never be executed" }
    for (j = 0; j < 10; ++ j)
      { case 1:; }
  }
  switch (i) // { dg-error "invalid entry to OpenMP structured block" }
  {
  #pragma omp critical // { dg-warning "statement will never be executed" }
    { case 2:; }
  }
  switch (i) // { dg-error "invalid entry to OpenMP structured block" }
  {
  #pragma omp master // { dg-warning "statement will never be executed" }
    { case 3:; }
  }
  switch (i) // { dg-error "invalid entry to OpenMP structured block" }
  {
  #pragma omp sections // { dg-warning "statement will never be executed" }
    { case 4:;
    #pragma omp section
       { case 5:; }
    }
  }
  switch (i) // { dg-error "invalid entry to OpenMP structured block" }
  {
  #pragma omp ordered // { dg-warning "statement will never be executed" }
    { default:; }
  }
}
