// Build don't link:

namespace N {}

void f(int N::k); // ERROR - 

class Foo
{
  int N::j; // ERROR - invalid use of `::'
};
