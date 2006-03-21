// PR c++/26690
// { dg-do compile }

struct A
{					// { dg-error "A::A\\(const A&\\)" }
  A (int);				// { dg-error "candidates" }
};

void
foo ()
{
  A a(0);
#pragma omp parallel private (a)	// { dg-error "no matching function" }
  ;
}
