extern void dummy (int);

void
test (void)
{
  #pragma omp for
  #pragma omp tile sizes(1, 2) /* { dg-error "non-rectangular 'tile'" "" { target c } } */
  for (int i = 0; i < 100; ++i) /* { dg-error "non-rectangular 'tile'" "" { target c++ } } */
    for (int j = i; j < 100; ++j)
      dummy (i);

  #pragma omp for
  #pragma omp tile sizes(1, 2) /* { dg-error "non-rectangular 'tile'" "" { target c } } */
  for (int i = 0; i < 100; ++i) /* { dg-error "non-rectangular 'tile'" "" { target c++ } } */
    for (int j = 0; j < i; ++j)
      dummy (i);


  #pragma omp for collapse(2)
  #pragma omp tile sizes(1) /* { dg-error "'tile' construct generates 1 loops with canonical form but 2 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp for collapse(3)
  #pragma omp tile sizes(1, 2) /* { dg-error "'tile' construct generates 2 loops with canonical form but 3 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp for collapse(2)
  #pragma omp tile sizes(1, 2)
  #pragma omp tile sizes(1) /* { dg-error "'tile' construct generates 1 loops with canonical form but 2 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp for collapse(2)
  #pragma omp tile sizes(1, 2)
  #pragma omp tile sizes(1, 2)
  for (int i = 0; i < 100; ++i) /* { dg-error "not enough nested loops" } */
    dummy (i);

  #pragma omp for collapse(2)
  #pragma omp tile sizes(5, 6)
  #pragma omp tile sizes(1, 2, 3)
  for (int i = 0; i < 100; ++i) /* { dg-error "not enough nested loops" } */
    dummy (i);

  #pragma omp for collapse(2)
  #pragma omp tile sizes(1, 2)
  #pragma omp tile sizes(1) /* { dg-error "'tile' construct generates 1 loops with canonical form but 2 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp for collapse(3)
  #pragma omp tile sizes(1, 2) /* { dg-error "'tile' construct generates 2 loops with canonical form but 3 loops are needed" } */
  #pragma omp tile sizes(1, 2)
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp for collapse(3)
  #pragma omp tile sizes(5, 6) /* { dg-error "'tile' construct generates 2 loops with canonical form but 3 loops are needed" } */
  #pragma omp tile sizes(1, 2, 3)
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      for (int k = 0; k < 100; ++k)
	dummy (i);
}
