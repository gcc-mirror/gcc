// Test parsing of #pragma omp declare variant
// { dg-do compile }

void f01 ();
#pragma omp declare variant (f01) match (user={condition((T) 1)})	// { dg-error "property must be constant integer expression" }
template <typename T>
void f02 ();
void f03 ();
#pragma omp declare variant (f03) match (user={condition(score((T) 1):1)})	// { dg-error "score argument must be constant integer expression" }
template <typename T>
void f04 ();
void f05 ();
#pragma omp declare variant (f05) match (user={condition(score(N):1)})	// { dg-error "score argument must be non-negative" }
template <int N>
void f06 ();
void f07 ();
#pragma omp declare variant (f05) match (user={condition(score(N):1)})
template <int N>
void f08 ();

void
test ()
{
  f02 <double> ();
  f04 <float> ();
  f06 <-1> ();
  f08 <0> ();
}
