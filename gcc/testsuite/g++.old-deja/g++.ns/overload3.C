// { dg-do assemble  }

namespace A{
  void f();
}

using A::f;

namespace A{
  void f(int);
}

using A::f;

void g()
{
  f(4);
}
