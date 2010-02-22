// { dg-do assemble  }
namespace A{
  void f();
}

namespace B{
  using namespace A;
  void f(int);		/* { dg-message "note: declared here" } */
}

using namespace B;

void g()
{
  ::f();               // { dg-error "" } A::f is not found
}

using namespace A;

void g1()
{
  ::f();               // ok, it is found now
}
