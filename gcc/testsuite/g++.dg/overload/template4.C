// PR c++/35986
// { dg-do compile }

namespace
{
  template <int> void foo (...);	// { dg-error "" "candidate" }
  template <int> void bar (int, ...);	// { dg-error "" "candidate" }
  void baz (...);			// { dg-error "" "candidate" }
}

template <int> void foo (...);		// { dg-error "" "candidate" }
template <int> void bar (int, ...);	// { dg-error "" "candidate" }
void baz (...);				// { dg-error "" "candidate" }

void
test ()
{
  foo <0> (0);		// { dg-error "is ambiguous" }
  bar <1> (0, 1);	// { dg-error "is ambiguous" }
  baz (0);		// { dg-error "is ambiguous" }
}
