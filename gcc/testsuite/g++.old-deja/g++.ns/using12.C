// Build don't link:
// Origin: Brendan Kehoe <brendan@cygnus.com>

namespace foo
{
  void x (bool);     // ERROR - candidates
  void x (char);     // ERROR - candidates
  void x (int);      // ERROR - candidates
  void x (double);   // ERROR - candidates
}

namespace baz { void x (int); }  // ERROR - candidates

void fn (int i)
{
  using foo::x;
  using baz::x;
  x(i); 	 // ERROR - ambiguous
}
