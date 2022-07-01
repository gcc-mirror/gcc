// PR c++/105734
// { dg-do compile { target c++11 } }

namespace N {
  struct A { };
  A f(A);
}

template <class T>
void bar() {
  auto m = f(T());
  m.~A();
}

void foo() { bar<N::A>(); }
