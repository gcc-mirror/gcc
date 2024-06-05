void
test (void)
{
#pragma omp tile sizes (2,4,6)
  for (unsigned i = 0; i < 10; i++) /* { dg-error "inner loops must be perfectly nested" } */
    for (unsigned j = 0; j < 10; j++)
      {
	float intervening_decl = 0;
#pragma omp unroll partial(2)
	for (unsigned k = 0; k < 10; k++);
      }
}
