extern void dummy (int);

void
test (void)
{
  #pragma omp tile sizes(1)
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp tile sizes(1)
  #pragma omp tile sizes(1)
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp tile sizes(1, 2)
  #pragma omp tile sizes(1, 2)
  for (int i = 0; i < 100; ++i)
  for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp tile sizes(5, 6)
  #pragma omp tile sizes(1, 2, 3)
  for (int i = 0; i < 100; ++i)
  for (int j = 0; j < 100; ++j)
  for (int k = 0; k < 100; ++k)
      dummy (i);

  #pragma omp tile sizes(1)
  #pragma omp unroll partial
  #pragma omp tile sizes(1)
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp tile sizes(1, 2)
  for (int i = 0; i < 100; ++i)
  for (int j = 0; j < 100; ++j)
    dummy (i);

  #pragma omp tile sizes(1)
  for (int i = 0; i < 100; ++i)
    {
      dummy (i);
      for (int j = 0; j < 100; ++j)
	dummy (i);
    }

  #pragma omp tile sizes(1)
  for (int i = 0; i < 100; ++i)
    {
      for (int j = 0; j < 100; ++j)
	dummy (j);
      dummy (i);
    }
}
