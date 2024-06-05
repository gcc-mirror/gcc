extern void dummy (int);

void
test1 (void)
{
  #pragma omp unroll partial
  for (int i = 0; i < 100; ++i)
    dummy (i);
}

void
test2 (void)
{
  #pragma omp unroll partial(10)
  for (int i = 0; i < 100; ++i)
    dummy (i);
}

void
test3 (void)
{
  #pragma omp unroll full
  for (int i = 0; i < 100; ++i)
    dummy (i);
}

void
test4 (void)
{
  #pragma omp unroll full
  for (int i = 0; i > 100; ++i)
    dummy (i);
}

void
test5 (void)
{
  #pragma omp unroll full
  for (int i = 1; i <= 100; ++i)
    dummy (i);
}

void
test6 (void)
{
  #pragma omp unroll full
  for (int i = 200; i >= 100; i--)
    dummy (i);
}

void
test7 (void)
{
  #pragma omp unroll full
  for (int i = -100; i > 100; ++i)
    dummy (i);
}

void
test8 (void)
{
  #pragma omp unroll full
  for (int i = 100; i > -200; --i)
    dummy (i);
}

void
test9 (void)
{
  #pragma omp unroll full
  for (int i = -300; i != 100; ++i)
    dummy (i);
}

void
test10 (void)
{
  #pragma omp unroll full
  for (int i = -300; i != 100; ++i)
    dummy (i);
}

void
test12 (void)
{
  #pragma omp unroll full
  #pragma omp unroll partial
  #pragma omp unroll partial
  for (int i = -300; i != 100; ++i)
    dummy (i);
}

void
test13 (void)
{
  for (int i = 0; i < 100; ++i)
    #pragma omp unroll full
    #pragma omp unroll partial
    #pragma omp unroll partial
    for (int j = -300; j != 100; ++j)
      dummy (i);
}

void
test14 (void)
{
  #pragma omp for
  for (int i = 0; i < 100; ++i)
    #pragma omp unroll full
    #pragma omp unroll partial
    #pragma omp unroll partial
    for (int j = -300; j != 100; ++j)
      dummy (i);
}

void
test15 (void)
{
  #pragma omp for
  for (int i = 0; i < 100; ++i)
    {
      dummy (i);

      #pragma omp unroll full
      #pragma omp unroll partial
      #pragma omp unroll partial
      for (int j = -300; j != 100; ++j)
	dummy (j);

      dummy (i);
    }
 }
