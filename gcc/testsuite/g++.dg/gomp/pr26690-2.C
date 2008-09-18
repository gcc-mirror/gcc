// PR c++/26690
// { dg-do compile }

struct A
{
  A (int x = 6);			// { dg-message "A::A\\(int\\)" }
  A (long long x = 12LL);		// { dg-message "candidates" }
};

void
foo ()
{
  A a(6);
#pragma omp parallel private (a)	// { dg-error "call of overloaded" }
  ;
}
