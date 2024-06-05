/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23" { target c } } */
/* { dg-prune-output "error: invalid controlling predicate" } */

extern void dummy (int);

void
test (void)
{
  [[omp::sequence (directive (unroll partial),
		   directive (unroll full))]] /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  [[omp::sequence (directive (for),
		   directive (unroll full), /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
		   directive (unroll partial))]]
  for (int i = -300; i != 100; ++i)
    dummy (i);

  [[omp::sequence (directive (for),
		   directive (unroll full),
		   directive (unroll full))]] /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  [[omp::sequence (directive (for),
		   directive (unroll partial partial))]] /* { dg-error "too many 'partial' clauses" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  [[omp::directive (unroll full full)]] /* { dg-error "too many 'full' clauses" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  [[omp::sequence (directive (unroll partial),
		   directive (unroll))]] /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  [[omp::sequence (directive (for),
		   directive (unroll))]] /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  int i;

  [[omp::sequence (directive (for), /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" "" { target *-*-* } .+1 } */
		   directive (unroll foo))]] /* { dg-error "expected an OpenMP clause before 'foo'" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  [[omp::directive (unroll partial(i))]]
    /* { dg-error "the value of 'i' is not usable in a constant expression" "" { target c++ } .-1 } */
    /* { dg-error "'partial' argument needs positive constant integer expression" "" { target *-*-* } .-2 } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  [[omp::directive (unroll parti)]] /* { dg-error "expected an OpenMP clause before 'parti'" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  [[omp::sequence (directive (for),
		   directive (unroll partial(1)), /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" "" { target *-*-* } .+1 } */
		   directive (unroll parti))]] /* { dg-error "expected an OpenMP clause before 'parti'" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  [[omp::sequence (directive (for),
		   directive (unroll partial(1)), /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" "" { target *-*-* } .+1 } */
		   directive (unroll parti))]] /* { dg-error "expected an OpenMP clause before 'parti'" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  int sum = 0;
  [[omp::sequence (directive (parallel for reduction(+ : sum) collapse(2)),
		   directive (unroll partial(1)))]] /* { dg-error "'unroll' construct with 'partial' clause generates just one loop with canonical form but 2 loops are needed" } */
  for (int i = 3; i < 10; ++i)
    for (int j = -2; j < 7; ++j)
      sum++;
}
