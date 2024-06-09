extern void dummy (int);

void
test (void)
{
  #pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i)
    #pragma omp tile sizes(2)
    for (int j = 0; j != 100; ++j)
      dummy (i);

  #pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i)
    #pragma omp tile sizes(2, 3)
    for (int j = 0; j != 100; ++j)
      for (int k = 0; k != 100; ++k)
	dummy (i);
}
