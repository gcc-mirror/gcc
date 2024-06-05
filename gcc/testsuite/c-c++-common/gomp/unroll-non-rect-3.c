extern void dummy (int);

void
test1 (void)
{
  #pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i)
    #pragma omp unroll partial(2)
    for (int j = i * 2; j <= i * 4 + 1; ++j) /* { dg-message "non-rectangular loops from generated loops unsupported" } */
      dummy (i);
}
