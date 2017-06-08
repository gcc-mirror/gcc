// PR c++/81011
// { dg-do compile }

class A { A (const A&); };		// { dg-message "declared private here" }
void foo (const A&);

void
bar (A& a)
{
#pragma omp task			// { dg-error "is private within this context" }
  foo (a);
}

void
baz (A& a)
{
#pragma omp task firstprivate (a)	// { dg-error "is private within this context" }
  foo (a);
}
