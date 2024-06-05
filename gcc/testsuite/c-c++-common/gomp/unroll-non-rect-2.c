extern void dummy (int);

void
test1 (void)
{
  #pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i)
    #pragma omp unroll partial
    for (int j = 2; j != i; ++j) /* { dg-message "non-rectangular loops from generated loops unsupported" } */
      dummy (i);
}

void
test2 (void)
{
  int i,j;
  #pragma omp target parallel for collapse(2)
  for (i = -300; i != 100; ++i)
    #pragma omp unroll partial
    for (j = 2; j != i; ++j) /* { dg-message "non-rectangular loops from generated loops unsupported" } */
      dummy (i);
}
