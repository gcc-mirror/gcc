extern void dummy (int);

void
test1 ()
{
#pragma omp target parallel for collapse(2) /* { dg-error {invalid OpenMP non-rectangular loop step; \'\(1 - 0\) \* 1\' is not a multiple of loop 2 step \'5\'} "" { target c } } */
  for (int i = -300; i != 100; ++i) /* { dg-error {invalid OpenMP non-rectangular loop step; \'\(1 - 0\) \* 1\' is not a multiple of loop 2 step \'5\'} "" { target c++ } } */
#pragma omp unroll partial
    for (int j = 2; j != i; ++j)
      dummy (i);
}

void
test2 ()
{
  int i,j;
#pragma omp target parallel for collapse(2)
  for (i = -300; i != 100; ++i)
    #pragma omp unroll partial
    for (j = 2; j != i; ++j)
      dummy (i);
}
