// PR c++/108116
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct A {
  A(int);
  ~A();
};

struct B {
  B(std::initializer_list<A>);
};

struct C {
  B m{0};
};

template<class>
void f() {
  C c = C{};
};
