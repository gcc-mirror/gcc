/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23" { target c } } */

extern void dummy (int);

void
test (void)
{
  [[omp::directive (tile sizes(1))]]
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::sequence (directive (tile sizes(1)),
		   directive (tile sizes(1)))]]
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::sequence (directive (tile sizes(1, 2)),
		   directive (tile sizes(1, 2)))]]
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  [[omp::sequence (directive (tile sizes(5, 6)),
		   directive (tile sizes(1, 2, 3)))]]
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    for (int k = 0; k < 100; ++k)
      dummy (i);

  [[omp::sequence (directive (tile sizes(1)),
		   directive (unroll partial),
		   directive (tile sizes(1)))]]
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::directive (tile sizes(1, 2))]]
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  [[omp::directive (tile sizes(1))]]
  for (int i = 0; i < 100; ++i)
    {
      dummy (i);
      for (int j = 0; j < 100; ++j)
	dummy (i);
    }

  [[omp::directive (tile sizes(1))]]
  for (int i = 0; i < 100; ++i)
    {
      for (int j = 0; j < 100; ++j)
	dummy (j);
      dummy (i);
    }
}
