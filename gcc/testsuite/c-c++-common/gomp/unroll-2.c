/* { dg-do compile { target { c || c++11 } } } */
/* { dg-prune-output "error: invalid controlling predicate" } */

extern void dummy (int);

void
test (void)
{
  #pragma omp unroll partial
  #pragma omp unroll full /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp for
  #pragma omp unroll full /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  #pragma omp unroll partial
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp for
  #pragma omp unroll full
  #pragma omp unroll full /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp for
  #pragma omp unroll partial partial /* { dg-error "too many 'partial' clauses" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp unroll full full /* { dg-error "too many 'full' clauses" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp unroll partial
  #pragma omp unroll /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp for
  #pragma omp unroll /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  int i;
  #pragma omp for /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" "" { target *-*-* } .+1 } */
  #pragma omp unroll( /* { dg-error "expected an OpenMP clause before '\\\(' token" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp for /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" "" { target *-*-* } .+1 } */
  #pragma omp unroll foo /* { dg-error "expected an OpenMP clause before 'foo'" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp unroll partial( /* { dg-error "expected expression before end of line" "" { target c } } */
  /* { dg-error "expected primary-expression before end of line" "" { target c++ } .-1 } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp unroll partial() /* { dg-error "expected expression before '\\\)' token" "" { target c } } */
  /* { dg-error "expected primary-expression before '\\\)' token" "" { target c++ } .-1 } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp unroll partial(i)
 /* { dg-error "the value of 'i' is not usable in a constant expression" "" { target c++ } .-1 } */
 /* { dg-error "'partial' argument needs positive constant integer expression" "" { target *-*-* } .-2 } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp unroll parti /* { dg-error "expected an OpenMP clause before 'parti'" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp for
  #pragma omp unroll partial(1) /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" "" { target *-*-* } .+1 } */
  #pragma omp unroll parti /* { dg-error "expected an OpenMP clause before 'parti'" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  #pragma omp for
  #pragma omp unroll partial(1) /* { dg-error "generated loop of 'unroll' construct without 'partial' clause does not have canonical form" "" { target *-*-* } .+1 } */
  #pragma omp unroll parti /* { dg-error "expected an OpenMP clause before 'parti'" } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  int sum = 0;
  #pragma omp parallel for reduction(+ : sum) collapse(2)
  #pragma omp unroll partial(1) /* { dg-error "'unroll' construct with 'partial' clause generates just one loop with canonical form but 2 loops are needed" } */
  for (int i = 3; i < 10; ++i)
    for (int j = -2; j < 7; ++j)
      sum++;

  #pragma omp unroll partial full /* { dg-error "'full' clause must not be used together with 'partial' clause" } */
  for (int i = 0; i < 42; ++i)
    dummy (i);

  #pragma omp unroll full partial /* { dg-error "'full' clause must not be used together with 'partial' clause" } */
  for (int i = 0; i < 42; ++i)
    dummy (i);

  #pragma omp unroll partial(7) full /* { dg-error "'full' clause must not be used together with 'partial' clause" } */
  for (int i = 0; i < 42; ++i)
    dummy (i);

  #pragma omp unroll full partial(28) /* { dg-error "'full' clause must not be used together with 'partial' clause" } */
  for (int i = 0; i < 42; ++i)
    dummy (i);

  #pragma omp unroll partial(0.5) /* { dg-error "'partial' argument needs positive constant integer expression" } */
  for (int i = 0; i < 42; ++i)
    dummy (i);

  #pragma omp unroll partial(0) /* { dg-error "'partial' argument needs positive constant integer expression" } */
  for (int i = 0; i < 42; ++i)
    dummy (i);

  #pragma omp unroll partial(-42) /* { dg-error "'partial' argument needs positive constant integer expression" } */
  for (int i = 0; i < 42; ++i)
    dummy (i);
}
