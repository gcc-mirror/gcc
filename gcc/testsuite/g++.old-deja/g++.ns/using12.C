// { dg-do assemble  }
// Origin: Brendan Kehoe <brendan@cygnus.com>

namespace foo
{
  void x (bool);     // { dg-error "" } candidates
  void x (char);     // { dg-error "" } candidates
  void x (int);      // { dg-error "" } candidates
  void x (double);   // { dg-error "" } candidates
}

namespace baz { void x (int); }  // { dg-error "" } candidates

void fn (int i)
{
  using foo::x;
  using baz::x;
  x(i); 	 // { dg-error "" } ambiguous
}
