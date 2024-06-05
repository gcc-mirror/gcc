extern void dummy (int);

void
test (void)
{
  #pragma omp for collapse(1)
  #pragma omp tile sizes(1)
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp for collapse(2)
  #pragma omp tile sizes(1, 2)
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp for collapse(1)
  #pragma omp tile sizes(1)
  #pragma omp tile sizes(1)
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp for collapse(1)
  #pragma omp tile sizes(1)
  #pragma omp tile sizes(1)
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp for collapse(2)
  #pragma omp tile sizes(1, 2)
  #pragma omp tile sizes(1, 2)
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp for collapse(2)
  #pragma omp tile sizes(5, 6)
  #pragma omp tile sizes(1, 2, 3)
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      for (int k = 0; k < 100; ++k)
	dummy (i);
}
