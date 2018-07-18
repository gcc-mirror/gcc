// PR c++/84557
// { dg-do compile }

template<int> struct A {};
template<int> struct B {};

void
foo ()
{
  #pragma omp parallel firstprivate (A)		// { dg-error "is not a variable in clause" }
  ;
  #pragma omp parallel firstprivate (B<0>)	// { dg-error "is not a variable in clause" }
  ;
}
