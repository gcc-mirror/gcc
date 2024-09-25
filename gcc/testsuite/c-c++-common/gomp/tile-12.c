extern void dummy (int);

void
test (void)
{
  #pragma omp parallel for
  #pragma omp tile sizes(0) /* { dg-error "'sizes' argument needs positive integral constant" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(-1) /* { dg-error "'sizes' argument needs positive integral constant" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp tile sizes(5, -42) /* { dg-error "'sizes' argument needs positive integral constant" } */
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp tile sizes(0.5f) /* { dg-error "'sizes' argument needs positive integral constant" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes() /* { dg-error "expected expression before" "" { target c} } */
  /* { dg-error "expected primary-expression before" "" { target c++ } .-1 } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(,) /* { dg-error "expected expression before" "" { target c } } */
  /* { dg-error "expected primary-expression before" "" { target c++ } .-1 } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(1,2 /* { dg-error "expected '\\\)' before end of line" "" { target c } } */
  for (int i = 0; i < 100; ++i) /* { dg-error "not enough nested loops" "" { target c } } */
    dummy (i); /* { dg-error "expected ',' before end of line" "" { target c++ } .-2 } */

  #pragma omp parallel for
  #pragma omp tile sizes /* { dg-error "expected '\\\(' before end of line" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(1) sizes(1) /* { dg-error "expected end of line before 'sizes'" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(1, 2)
  #pragma omp tile sizes(1) /* { dg-error "'tile' construct generates 1 loops with canonical form but 2 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(1)
  #pragma omp unroll partia /* { dg-error "expected an OpenMP clause before 'partia'" } */
  #pragma omp tile sizes(1) /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" "" { target *-*-* } .-1 } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(1)
  #pragma omp unroll /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(1)
  #pragma omp unroll full /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(8,8)
  #pragma omp unroll partial /* { dg-error "'unroll' construct with 'partial' clause generates just one loop with canonical form but 2 loops are needed" } */
  #pragma omp tile sizes(1)
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(8,8)
  #pragma omp unroll partial /* { dg-error "'unroll' construct with 'partial' clause generates just one loop with canonical form but 2 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(1, 2) /* { dg-error "non-rectangular 'tile'" "" { target c } } */
  for (int i = 0; i < 100; ++i) /* { dg-error "non-rectangular 'tile'" "" { target c++ } } */
    for (int j = i; j < 100; ++j)
      dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(1, 2) /* { dg-error "non-rectangular 'tile'" "" { target c } } */
  for (int i = 0; i < 100; ++i) /* { dg-error "non-rectangular 'tile'" "" { target c++ } } */
    for (int j = 2; j < i; ++j)
      dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(1, 2, 3)
  for (int i = 0; i < 100; ++i) /* { dg-error "not enough nested loops" } */
    for (int j = 0; j < 100; ++j)
      dummy (i);

  #pragma omp parallel for
  #pragma omp tile sizes(1, 2)
  for (int i = 0; i < 100; ++i) /* { dg-error "inner loops must be perfectly nested" } */
    {
      dummy (i);
      for (int j = 0; j < 100; ++j)
	dummy (j);
    }

  #pragma omp parallel for
  #pragma omp tile sizes(1, 2)
  for (int i = 0; i < 100; ++i) /* { dg-error "inner loops must be perfectly nested" } */
    {
      for (int j = 0; j < 100; ++j)
	dummy (j);
      dummy (i);
    }

  #pragma omp tile /* { dg-error "expected 'sizes'" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  #pragma omp tile sizes (1) sizes (1) /* { dg-error "expected end of line" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);
}
