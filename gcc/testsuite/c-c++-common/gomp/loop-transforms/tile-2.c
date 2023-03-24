extern void dummy (int);

void
test ()
{
    #pragma omp parallel for
    #pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(0) /* { dg-error {'tile sizes' argument needs positive integral constant} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(-1) /* { dg-error {'tile sizes' argument needs positive integral constant} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes() /* { dg-error {expected expression before} "" { target c} } */
    /* { dg-error {expected primary-expression before} "" { target c++ } .-1 } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(,) /* { dg-error {expected expression before} "" { target c } } */
    /* { dg-error {expected primary-expression before} "" { target c++ } .-1 } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1,2 /* { dg-error {expected '\,' before end of line} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes /* { dg-error {expected '\(' before end of line} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1) sizes(1) /* { dg-error {expected end of line before 'sizes'} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1)
    #pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1, 2)
    #pragma omp tile sizes(1) /* { dg-error {nesting depth left after this transformation too low for outer transformation} } */
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1, 2)
    #pragma omp tile sizes(1, 2)
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(5, 6)
    #pragma omp tile sizes(1, 2, 3)
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    for (int k = 0; k < 100; ++k)
    	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1)
    #pragma omp unroll partia /* { dg-error {expected '#pragma omp' clause before 'partia'} } */
    #pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1)
    #pragma omp unroll /* { dg-error {'#pragma omp unroll' without 'partial' clause is invalid here; turns loop into non-loop} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1)
    #pragma omp unroll full /* { dg-error {'full' clause is invalid here; turns loop into non-loop} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1)
    #pragma omp unroll partial
    #pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(8,8)
    #pragma omp unroll partial /* { dg-error {nesting depth left after this transformation too low for outer transformation} } */
    #pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(8,8)
    #pragma omp unroll partial /* { dg-error {nesting depth left after this transformation too low for outer transformation} } */
    for (int i = 0; i < 100; ++i)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1, 2)
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1, 2) /* { dg-error {'tile' loop transformation may not appear on non-rectangular for} } */
    for (int i = 0; i < 100; ++i)
    for (int j = i; j < 100; ++j)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1, 2) /* { dg-error {'tile' loop transformation may not appear on non-rectangular for} } */
    for (int i = 0; i < 100; ++i)
    for (int j = 2; j < i; ++j)
	dummy (i);

    #pragma omp parallel for
    #pragma omp tile sizes(1, 2, 3)
    for (int i = 0; i < 100; ++i)
      for (int j = 0; j < 100; ++j)
        dummy (i); /* { dg-error {not enough perfectly nested loops before 'dummy'} "" { target c } } */
    /* { dg-error {not enough for loops to collapse} "" { target c++ } .-1 } */
    /* { dg-error {'i' was not declared in this scope} "" { target c++ } .-2 } */

    #pragma omp parallel for
    #pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
      {
	dummy (i);
        for (int j = 0; j < 100; ++j)
          dummy (i);
      }

    #pragma omp parallel for
    #pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
      {
        for (int j = 0; j < 100; ++j)
	    dummy (j);
	dummy (i);
      }

    #pragma omp parallel for
    #pragma omp tile sizes(1, 2)
    for (int i = 0; i < 100; ++i)
      {
        dummy (i); /* { dg-error {not enough perfectly nested loops before 'dummy'} "" { target c } } */
	/* { dg-error {not enough for loops to collapse} "" { target c++ } .-1 } */
        for (int j = 0; j < 100; ++j)
          dummy (j);
      }

    #pragma omp parallel for
    #pragma omp tile sizes(1, 2)
    for (int i = 0; i < 100; ++i)
      {
        for (int j = 0; j < 100; ++j)
	  dummy (j);
	dummy (i); /* { dg-error {collapsed loops not perfectly nested before 'dummy'} "" { target c} } */
	/* { dg-error {collapsed loops not perfectly nested} "" { target c++ } .-1 } */
      }

    #pragma omp parallel for
    #pragma omp tile sizes(1)
    for (int i = 0; i < 100; ++i)
	dummy (i);
}
