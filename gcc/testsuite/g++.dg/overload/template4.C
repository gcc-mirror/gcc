// PR c++/35986
// { dg-do compile }

namespace
{
  template <int> void foo (...);	// { dg-message "foo" }
  template <int> void bar (int, ...);	// { dg-message "bar" }
  void baz (...);			// { dg-message "baz" }
}

template <int> void foo (...);		// { dg-message "note" }
template <int> void bar (int, ...);	// { dg-message "note" }
void baz (...);				// { dg-message "note" }

void
test ()
{
  foo <0> (0);		// { dg-error "is ambiguous" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 18 }
  bar <1> (0, 1);	// { dg-error "is ambiguous" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 20 }
  baz (0);		// { dg-error "is ambiguous" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 22 }
}
