// Build don't link: 
namespace A{
  void f();
}

namespace B{
  using namespace A;
  void f(int);			// ERROR - referenced below
}

using namespace B;

void g()
{
  ::f();               // ERROR - A::f is not found
}

using namespace A;

void g1()
{
  ::f();               // ok, it is found now
}
