// PR c++/13659

// { dg-do compile }

namespace foo {
  template <class T> void f(T, T);
}
namespace bar {
  using namespace foo __attribute__((strong));
  template <class T> void f(T);
}

int main() {
  // Make sure both declarations are brought in.
  using bar::f;
  f(1);
  f(1, 1);
}
