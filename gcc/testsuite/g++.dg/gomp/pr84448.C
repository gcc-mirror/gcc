// PR c++/84448
// { dg-do compile }

struct A
{
  operator int () const;
  A& operator += (int);
  A& operator ++ ();
};

void
foo (A a, A b)
{
  #pragma omp for
  for (A i = a; i <=; ++i)	// { dg-error "expected primary-expression before" }
    ;				// { dg-error "invalid controlling predicate" "" { target *-*-* } .-1 }
}
