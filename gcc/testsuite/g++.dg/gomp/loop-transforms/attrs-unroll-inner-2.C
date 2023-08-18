// { dg-do compile { target c++11 } }

extern void dummy (int);

void
test ()
{

#pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i)
    [[omp::directive (tile sizes (2))]]
    for (int j = 0; j != 100; ++j)
      dummy (i);

  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i) /* { dg-error {not enough nested loops} } */
    [[omp::directive (tile sizes(2, 3))]]
    for (int j = 0; j != 100; ++j)
      dummy (i);

  [[omp::directive (target parallel for, collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::directive (tile, sizes(2, 3))]]
    for (int j = 0; j != 100; ++j)
      for (int k = 0; k != 100; ++k)
	dummy (i);
}


