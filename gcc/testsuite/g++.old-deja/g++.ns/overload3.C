// Build don't link:
// Declarations after the first one don't affect the set of used decls.

namespace A{
  void f();         // ERROR - .*
}

using A::f;

namespace A{
  void f(int);
}

using A::f;

void g()
{
  f(4);            // ERROR - too many arguments
}
