void
foo (void)
{
  int i;
  #pragma omp for schedule(static, 1)
  for (i = 0; i < 10; i++)
    ;
  #pragma omp for schedule(static, 0)		/* { dg-warning "chunk size value must be positive" } */
  for (i = 0; i < 10; i++)
    ;
  #pragma omp for schedule(static, -7)		/* { dg-warning "chunk size value must be positive" } */
  for (i = 0; i < 10; i++)
    ;
}
