void
foo (void)
{
  #pragma omp for collapse(2)	/* { dg-error "invalid OpenMP non-rectangular loop step" } */
  for (int i = 0; i < 6; i++)
    for (int j = 4 * i; j < 7 * i; j += 2)
      ;
  #pragma omp for collapse(2)	/* { dg-error "invalid OpenMP non-rectangular loop step" } */
  for (int i = 0; i < 32; i += 7)
    for (int j = 3 * i; j < 7 * i; j += 30)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 6; i++)
    for (int j = 4 * i; j < 6 * i; j += 2)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 6; i += 2)
    for (int j = 4 * i; j < 7 * i; j += 2)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 6; i += 5)
    for (int j = 4 * i; j < 7 * i; j += 15)
      ;
}
