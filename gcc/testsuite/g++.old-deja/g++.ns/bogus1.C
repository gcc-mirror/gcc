// Build don't link:

namespace N {}

void f(int N::k); // ERROR - cannot use `::' in parameter declaration

class Foo
{
  int N::j; // ERROR - invalid use of `::'
};
