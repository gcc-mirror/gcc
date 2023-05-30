/* { dg-prune-output "error: invalid controlling predicate" } */
/* { dg-additional-options "-std=c++11" { target c++} } */

extern void dummy (int);

void
test ()
{
#pragma omp unroll partial
#pragma omp unroll full /* { dg-error {'full' clause is invalid here; turns loop into non-loop} } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp for
#pragma omp unroll full /* { dg-error {'full' clause is invalid here; turns loop into non-loop} } */
#pragma omp unroll partial
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp for
#pragma omp unroll full /* { dg-error {'full' clause is invalid here; turns loop into non-loop} } */
#pragma omp unroll full
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp for
#pragma omp unroll partial partial /* { dg-error {too many 'partial' clauses} } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp unroll full full /* { dg-error {too many 'full' clauses} } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp unroll partial
#pragma omp unroll /* { dg-error {'#pragma omp unroll' without 'partial' clause is invalid here; turns loop into non-loop} } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp for
#pragma omp unroll /* { dg-error {'#pragma omp unroll' without 'partial' clause is invalid here; turns loop into non-loop} } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

  int i;
#pragma omp for
#pragma omp unroll( /* { dg-error {expected an OpenMP clause before '\(' token} } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp for
#pragma omp unroll foo /* { dg-error {expected an OpenMP clause before 'foo'} } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp unroll partial( /* { dg-error {expected expression before end of line} "" { target c } } */
  /* { dg-error {expected primary-expression before end of line} "" { target c++ } .-1 } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp unroll partial() /* { dg-error {expected expression before '\)' token} "" { target c } } */
  /* { dg-error {expected primary-expression before '\)' token} "" { target c++ } .-1 } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp unroll partial(i)
 /* { dg-error {the value of 'i' is not usable in a constant expression} "" { target c++ } .-1 } */
 /* { dg-error {partial argument needs positive constant integer expression} "" { target *-*-* } .-2 } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp unroll parti /* { dg-error {expected an OpenMP clause before 'parti'} } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp for
#pragma omp unroll partial(1)
#pragma omp unroll parti /* { dg-error {expected an OpenMP clause before 'parti'} } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

#pragma omp for
#pragma omp unroll partial(1)
#pragma omp unroll parti /* { dg-error {expected an OpenMP clause before 'parti'} } */
  for (int i = -300; i != 100; ++i)
    dummy (i);

int sum = 0;
#pragma omp parallel for reduction(+ : sum) collapse(2)
#pragma omp unroll partial(1) /* { dg-error {nesting depth left after this transformation too low for loop collapse} } */
  for (int i = 3; i < 10; ++i)
    for (int j = -2; j < 7; ++j)
      sum++;
}

