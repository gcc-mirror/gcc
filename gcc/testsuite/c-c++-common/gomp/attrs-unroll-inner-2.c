/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23" { target c } } */

extern void dummy (int);

void
test (void)
{
  #pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i)
    [[omp::directive (tile sizes (2))]]
    for (int j = 0; j != 100; ++j)
      dummy (i);

  [[omp::directive (target parallel for, collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::directive (tile, sizes(2, 3))]]
    for (int j = 0; j != 100; ++j)
      for (int k = 0; k != 100; ++k)
	dummy (i);
}
