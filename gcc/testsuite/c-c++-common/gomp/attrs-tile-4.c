/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23" { target c } } */

extern void dummy (int);

void
test (void)
{
  [[omp::directive (tile sizes(0))]] /* { dg-error "'sizes' argument needs positive integral constant" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::directive (tile sizes(-1))]] /* { dg-error "'sizes' argument needs positive integral constant" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::directive (tile sizes())]] /* { dg-error "expected expression before" "" { target c } } */
  for (int i = 0; i < 100; ++i) /* { dg-error "expected primary-expression before" "" { target c++ } .-1 } */
    dummy (i);

  [[omp::directive (tile sizes)]] /* { dg-error "expected '\\\(' before end of line" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::directive (tile sizes(1) sizes(1))]] /* { dg-error "expected end of line before 'sizes'" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::directive (tile, sizes(1), sizes(1))]] /* { dg-error "expected end of line before ','" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::sequence (directive (tile sizes(1, 2)),
		   directive (tile sizes(1)))]] /* { dg-error "'tile' construct generates 1 loops with canonical form but 2 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  [[omp::sequence (directive (tile sizes(1)),
		   directive (unroll partia), /* { dg-error "expected an OpenMP clause before 'partia'" } */
		   directive (tile sizes(1)))]] /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" "" { target *-*-* } .-1 } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::sequence (directive (tile sizes(1)),
		   directive (unroll))]] /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::sequence (directive (tile sizes(1)),
		   directive (unroll full))]] /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::sequence (directive (tile sizes(8,8)),
		   directive (unroll partial), /* { dg-error "'unroll' construct with 'partial' clause generates just one loop with canonical form but 2 loops are needed" } */
		   directive (tile sizes(1)))]]
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::sequence (directive (tile sizes(8,8)),
		   directive (unroll partial))]] /* { dg-error "'unroll' construct with 'partial' clause generates just one loop with canonical form but 2 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::directive (tile sizes(1, 2))]] /* { dg-error "non-rectangular 'tile'" } */
  for (int i = 0; i < 100; ++i)
    for (int j = i; j < 100; ++j)
      dummy (i);

  [[omp::directive (tile sizes(1, 2))]] /* { dg-error "non-rectangular 'tile'" } */
  for (int i = 0; i < 100; ++i)
    for (int j = 2; j < i; ++j)
      dummy (i);

  [[omp::directive (tile sizes(1, 2, 3))]]
  for (int i = 0; i < 100; ++i) /* { dg-error "not enough nested loops" } */
    for (int j = 0; j < 100; ++j)
      dummy (i);

  [[omp::directive (tile sizes(1, 2))]]
  for (int i = 0; i < 100; ++i) /* { dg-error "inner loops must be perfectly nested" } */
    {
      dummy (i);
      for (int j = 0; j < 100; ++j)
	dummy (j);
    }

  [[omp::directive (tile sizes(1, 2))]]
  for (int i = 0; i < 100; ++i) /* { dg-error "inner loops must be perfectly nested" } */
    {
      for (int j = 0; j < 100; ++j)
	dummy (j);
      dummy (i);
    }

  int s;
  [[omp::directive (tile sizes(s))]] /* { dg-error "'sizes' argument needs positive integral constant" "" { target { ! c++98_only } } } */
  /* { dg-error "the value of 's' is not usable in a constant expression" "" { target { c++ && { ! c++98_only } } } .-1 } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::directive (tile sizes(42.0))]] /* { dg-error "'sizes' argument needs positive integral constant" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);
}
