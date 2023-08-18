// { dg-do compile { target c++11 } } 

extern void dummy (int);

void
test ()
{
    [[omp::directive (tile sizes(1))]]
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::directive (tile sizes(0))]] /* { dg-error {'tile sizes' argument needs positive integral constant} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::directive (tile sizes(-1))]] /* { dg-error {'tile sizes' argument needs positive integral constant} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::directive (tile sizes())]] /* { dg-error {expected primary-expression before} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::directive (tile sizes)]] /* { dg-error {expected '\(' before end of line} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::directive (tile sizes(1) sizes(1))]] /* { dg-error {expected end of line before 'sizes'} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::directive (tile, sizes(1), sizes(1))]] /* { dg-error {expected end of line before ','} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::sequence
      (directive (tile sizes(1)),
       directive (tile sizes(1)))]]
    for (int i = 0; i < 100; ++i)
      dummy (i);

    [[omp::sequence 
      (directive (tile sizes(1, 2)),
       directive (tile sizes(1)))]] /* { dg-error {nesting depth left after this transformation too low for outer transformation} } */
    for (int i = 0; i < 100; ++i)
      for (int j = 0; j < 100; ++j)
    	dummy (i);

    [[omp::sequence
      (directive (tile sizes(1, 2)),
       directive (tile sizes(1, 2)))]]
    for (int i = 0; i < 100; ++i)
      for (int j = 0; j < 100; ++j)
    	dummy (i);

    [[omp::sequence
      (directive (tile sizes(5, 6)),
       directive (tile sizes(1, 2, 3)))]]
    for (int i = 0; i < 100; ++i)
      for (int j = 0; j < 100; ++j)
	for (int k = 0; k < 100; ++k)
	  dummy (i);

    [[omp::sequence
      (directive (tile sizes(1)),
       directive (unroll partia), /* { dg-error {expected an OpenMP clause before 'partia'} } */
       directive (tile sizes(1)))]]
    for (int i = 0; i < 100; ++i)
      dummy (i);

    [[omp::sequence
      (directive (tile sizes(1)),
       directive (unroll))]] /* { dg-error {'#pragma omp unroll' without 'partial' clause is invalid here; turns loop into non-loop} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::sequence
      (directive (tile sizes(1)),
       directive (unroll full))]] /* { dg-error {'full' clause is invalid here; turns loop into non-loop} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::sequence
      (directive (tile sizes(1)),
       directive (unroll partial),
       directive (tile sizes(1)))]]
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::sequence
      (directive (tile sizes(8,8)),
       directive (unroll partial), /* { dg-error {nesting depth left after this transformation too low for outer transformation} } */
       directive (tile sizes(1)))]]
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::sequence
      (directive (tile sizes(8,8)),
       directive (unroll partial))]] /* { dg-error {nesting depth left after this transformation too low for outer transformation} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::directive (tile sizes(1, 2))]]
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
	dummy (i);

    [[omp::directive (tile sizes(1, 2))]] /* { dg-error {'tile' loop transformation may not appear on non-rectangular for} } */
    for (int i = 0; i < 100; ++i)
    for (int j = i; j < 100; ++j)
	dummy (i);

    [[omp::directive (tile sizes(1, 2))]] /* { dg-error {'tile' loop transformation may not appear on non-rectangular for} } */
    for (int i = 0; i < 100; ++i)
    for (int j = 2; j < i; ++j)
	dummy (i);

    [[omp::directive (tile sizes(1, 2, 3))]]
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} } */
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

    [[omp::directive (tile sizes(1, 2))]]
    for (int i = 0; i < 100; ++i) /* { dg-error {inner loops must be perfectly nested} } */
      {
        dummy (i);
        for (int j = 0; j < 100; ++j)
          dummy (j);
      }

    [[omp::directive (tile sizes(1, 2))]]
    for (int i = 0; i < 100; ++i) /* { dg-error {inner loops must be perfectly nested} } */
      {
        for (int j = 0; j < 100; ++j)
	  dummy (j);
	dummy (i);
      }

    int s;
    [[omp::directive (tile sizes(s))]] /* { dg-error {'tile sizes' argument needs positive integral constant} "" { target { ! c++98_only } } } */
    /* { dg-error {the value of 's' is not usable in a constant expression} "" { target { c++ && { ! c++98_only } } } .-1 } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::directive (tile sizes(42.0))]] /* { dg-error {'tile sizes' argument needs integral type} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);
}
