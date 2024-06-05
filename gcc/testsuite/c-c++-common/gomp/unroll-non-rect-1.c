extern void dummy (int);

void
test1 (void)
{
  #pragma omp target parallel for collapse(3)
  for (int i = -300; i != 100; ++i)
    for (int j = i; j != i * 2; ++j)
      #pragma omp unroll partial
      for (int k = 2; k != 100; ++k)
	dummy (i);

  #pragma omp unroll full
  for (int i = -300; i != 100; ++i)
    for (int j = i; j != i * 2; ++j)
      for (int k = 2; k != 100; ++k)
	dummy (i);

  for (int i = -300; i != 100; ++i)
    #pragma omp unroll full
    for (int j = i; j != i + 10; ++j)
      for (int k = 2; k != 100; ++k)
	dummy (i);

  for (int i = -300; i != 100; ++i)
    #pragma omp unroll full
    for (int j = i; j != i + 10; ++j)
      for (int k = j; k != 100; ++k)
	dummy (i);
}
