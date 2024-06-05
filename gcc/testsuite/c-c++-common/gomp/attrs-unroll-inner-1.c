/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23" { target c } } */

extern void dummy (int);

void
test (void)
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::directive (unroll, partial)]]
    for (int j = 0; j != 100; ++j)
      dummy (i);
}
