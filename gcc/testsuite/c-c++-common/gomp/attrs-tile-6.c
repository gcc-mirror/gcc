/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23" { target c } } */

extern void dummy (int);

void
test (void)
{
  [[omp::sequence (directive (for),
		   directive (tile sizes(1, 2)))]] /* { dg-error "non-rectangular 'tile'" "" { target c } } */
  for (int i = 0; i < 100; ++i) /* { dg-error "non-rectangular 'tile'" "" { target c++ } } */
    for (int j = i; j < 100; ++j)
      dummy (i);

  [[omp::sequence (directive (for),
		   directive (tile sizes(1, 2)))]] /* { dg-error "non-rectangular 'tile'" "" { target c } } */
  for (int i = 0; i < 100; ++i) /* { dg-error "non-rectangular 'tile'" "" { target c++ } } */
    for (int j = 0; j < i; ++j)
      dummy (i);

  [[omp::sequence (directive (for collapse(2)),
		   directive (tile sizes(1)))]] /* { dg-error "'tile' construct generates 1 loops with canonical form but 2 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  [[omp::sequence (directive (for collapse(3)),
		   directive (tile sizes(1, 2)))]] /* { dg-error "'tile' construct generates 2 loops with canonical form but 3 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  [[omp::sequence (directive (for collapse(2)),
		   directive (tile sizes(1, 2)),
		   directive (tile sizes(1)))]] /* { dg-error "'tile' construct generates 1 loops with canonical form but 2 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    dummy (i);

  [[omp::sequence (directive (for collapse(2)),
		   directive (tile sizes(1, 2)),
		   directive (tile sizes(1, 2)))]]
  for (int i = 0; i < 100; ++i) /* { dg-error "not enough nested loops" } */
    dummy (i);

  [[omp::sequence (directive (for collapse(2)),
		   directive (tile sizes(5, 6)),
		   directive (tile sizes(1, 2, 3)))]]
  for (int i = 0; i < 100; ++i) /* { dg-error "not enough nested loops" } */
    dummy (i);

  [[omp::sequence (directive (for collapse(2)),
		   directive (tile sizes(1, 2)),
		   directive (tile sizes(1)))]] /* { dg-error "'tile' construct generates 1 loops with canonical form but 2 loops are needed" } */
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  [[omp::sequence (directive (for collapse(3)),
		   directive (tile sizes(1, 2)), /* { dg-error "'tile' construct generates 2 loops with canonical form but 3 loops are needed" } */
		   directive (tile sizes(1, 2)))]]
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      dummy (i);

  [[omp::sequence (directive (for collapse(3)),
		   directive (tile sizes(5, 6)), /* { dg-error "'tile' construct generates 2 loops with canonical form but 3 loops are needed" } */
		   directive (tile sizes(1, 2, 3)))]]
  for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 100; ++j)
      for (int k = 0; k < 100; ++k)
	dummy (i);
}
