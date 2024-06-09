extern void dummy (int);

void
test (void)
{
  #pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i)
    #pragma omp unroll partial
    for (int j = 0; j != 100; ++j)
      dummy (i);
}
