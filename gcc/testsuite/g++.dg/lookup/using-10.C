// PR c++/13659

// { dg-do compile }

namespace foo1 {
  template <class T> void f(T);
}
namespace foo2 {
  template <class T> void f(T, T);
}
namespace foo {
  using namespace foo1;
  using namespace foo2;
}

// Make sure we bring in both declarations.
using foo::f;

int main() {
  f(1);
  f(1, 1);
}
