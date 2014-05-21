// { dg-do assemble  }
// Origin: Brendan Kehoe <brendan@cygnus.com>

namespace foo
{
  void x (bool);     // { dg-message "note" }
  void x (char);     // { dg-message "note" } candidates
  void x (int);      // { dg-message "note" } candidates
  void x (double);   // { dg-message "note" } candidates
}

namespace baz { void x (int); }  // { dg-message "note" }  candidates

void fn (int i)
{
  using foo::x;
  using baz::x;
  x(i); 	 // { dg-error "ambiguous" }
}
