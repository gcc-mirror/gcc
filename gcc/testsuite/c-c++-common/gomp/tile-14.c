extern void dummy (int);

void
test (void)
{
  #pragma omp for
  #pragma omp tile sizes(2, 3)
  #pragma omp tile sizes(3, 4, 5)
  #pragma omp tile sizes(6, 7, 8, 9)
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      for (int k = 0; k < 100; ++k)
	for (int l = 0; l < 100; ++l)
	  dummy (i);

  #pragma omp for
  #pragma omp tile sizes(2, 3)
  for (int i = 0; i < 100; ++i)
    #pragma omp tile sizes(3, 4, 5)
    for (int j = 0; j < 100; ++j)
      #pragma omp tile sizes(6, 7, 8, 9)
      for (int k = 0; k < 100; ++k)
	for (int l = 0; l < 100; ++l)
	  for (int m = 0; m < 100; ++m)
	    #pragma omp unroll partial(2)
	    for (int n = 0; n < 100; ++n)
	      dummy (i);

  #pragma omp for collapse(2)
  for (int i = 0; i < 100; ++i)
    #pragma omp tile sizes(2, 3)
    #pragma omp tile sizes(3, 4, 5)
    #pragma omp tile sizes(6, 7, 8, 9)
    for (int j = 0; j < 100; ++j)
      for (int k = 0; k < 100; ++k)
	for (int l = 0; l < 100; ++l)
	  for (int m = 0; m < 100; ++m)
	    dummy (i);

  #pragma omp for collapse(2)
  for (int i = 0; i < 100; ++i)
    #pragma omp tile sizes(2, 3)
    for (int j = 0; j < 100; ++j)
      #pragma omp tile sizes(3, 4, 5)
      for (int k = 0; k < 100; ++k)
	#pragma omp tile sizes(6, 7, 8, 9)
	for (int l = 0; l < 100; ++l)
	  for (int m = 0; m < 100; ++m)
	    for (int n = 0; n < 100; ++n)
	      #pragma omp unroll partial(2)
	      for (int o = 0; o < 100; ++o)
		dummy (i);
}
