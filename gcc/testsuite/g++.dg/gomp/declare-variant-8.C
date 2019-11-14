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

void
test ()
{
  f02 <double> ();
  f04 <float> ();
}
