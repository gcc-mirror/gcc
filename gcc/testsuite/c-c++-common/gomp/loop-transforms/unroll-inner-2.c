/* { dg-additional-options "-std=c++11" { target c++} } */

extern void dummy (int);

void
test ()
{

#pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i)
#pragma omp tile sizes(2)
    for (int j = 0; j != 100; ++j)
      dummy (i);

#pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i) /* { dg-error {not enough nested loops} } */
#pragma omp tile sizes(2, 3)
    for (int j = 0; j != 100; ++j)
      dummy (i);

#pragma omp target parallel for collapse(2)
  for (int i = -300; i != 100; ++i)
#pragma omp tile sizes(2, 3)
    for (int j = 0; j != 100; ++j)
      for (int k = 0; k != 100; ++k)
	dummy (i);
}


