// PR c++/101516

void
foo (int (&v) [])
{
  #pragma omp parallel reduction (+:v)	// { dg-error "invalid use of array with unspecified bounds" }
  ;
}
