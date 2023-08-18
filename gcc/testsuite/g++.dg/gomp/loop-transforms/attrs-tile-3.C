// { dg-do compile { target c++11 } }

extern void dummy (int);

void
test ()
{
    [[omp::sequence (directive (for),
      directive (tile sizes(1, 2)))]] /* { dg-error {'tile' loop transformation may not appear on non-rectangular for} } */
    for (int i = 0; i < 100; ++i)
    for (int j = i; j < 100; ++j)
	dummy (i);

    [[omp::sequence (directive (for),
      directive (tile sizes(1, 2)))]] /* { dg-error {'tile' loop transformation may not appear on non-rectangular for} } */
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < i; ++j)
	dummy (i);


    [[omp::sequence (directive (for collapse(1)),
      directive (tile sizes(1)))]]
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
	dummy (i);

    [[omp::sequence (directive (for collapse(2)),
      directive (tile sizes(1)))]] /* { dg-error {nesting depth left after this transformation too low for loop collapse} } */
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
	dummy (i);

    [[omp::sequence (directive (for collapse(2)),
      directive (tile sizes(1, 2)))]]
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
	dummy (i);

    [[omp::sequence (directive (for collapse(3)),
      directive (tile sizes(1, 2)))]] /* { dg-error {nesting depth left after this transformation too low for loop collapse} } */
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} } */
    for (int j = 0; j < 100; ++j)
	dummy (i);

    [[omp::sequence (directive (for collapse(1)),
      directive (tile sizes(1)),
      directive (tile sizes(1)))]]
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::sequence (directive (for collapse(2)),
      directive (tile sizes(1, 2)),
      directive (tile sizes(1)))]] /* { dg-error {nesting depth left after this transformation too low for outer transformation} } */
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} } */
    	dummy (i);

    [[omp::sequence (directive (for collapse(2)),
      directive (tile sizes(1, 2)),
      directive (tile sizes(1, 2)))]]
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} } */
    	dummy (i);

    [[omp::sequence (directive (for collapse(2)),
      directive (tile sizes(5, 6)),
      directive (tile sizes(1, 2, 3)))]]
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} } */
    	dummy (i); 

    [[omp::sequence (directive (for collapse(1)),
      directive (tile sizes(1)),
      directive (tile sizes(1)))]]
    for (int i = 0; i < 100; ++i)
	dummy (i);

    [[omp::sequence (directive (for collapse(2)),
      directive (tile sizes(1, 2)),
      directive (tile sizes(1)))]] /* { dg-error {nesting depth left after this transformation too low for outer transformation} } */
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    	dummy (i);

    [[omp::sequence (directive (for collapse(2)),
      directive (tile sizes(1, 2)),
      directive (tile sizes(1, 2)))]]
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    	dummy (i);

    [[omp::sequence (directive (for collapse(2)),
      directive (tile sizes(5, 6)),
      directive (tile sizes(1, 2, 3)))]]
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    for (int k = 0; k < 100; ++k)
    	dummy (i);

    [[omp::sequence (directive (for collapse(3)),
      directive (tile sizes(1, 2)), /* { dg-error {nesting depth left after this transformation too low for loop collapse} } */
      directive (tile sizes(1, 2)))]]
    for (int i = 0; i < 100; ++i) /* { dg-error {not enough nested loops} } */
    for (int j = 0; j < 100; ++j)
    	dummy (i);

    [[omp::sequence (directive (for collapse(3)),
      directive (tile sizes(5, 6)), /* { dg-error {nesting depth left after this transformation too low for loop collapse} } */
      directive (tile sizes(1, 2, 3)))]]
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
    for (int k = 0; k < 100; ++k)
    	dummy (i);
}
