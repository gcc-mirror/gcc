extern void dummy (int);

void
test (void)
{
  #pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i)
    #pragma omp tile sizes(2, 3)
    for (int j = 0; j != 100; ++j) /* { dg-error "not enough nested loops" } */
      dummy (i);
}
