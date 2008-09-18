// PR c++/26690
// { dg-do compile }

struct A
{					// { dg-message "A::A\\(const A&\\)" }
  A (int);				// { dg-message "candidates" }
};

void
foo ()
{
  A a(0);
#pragma omp parallel private (a)	// { dg-error "no matching function" }
  ;
}
