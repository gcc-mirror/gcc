// { dg-do compile { target c++14 } }

struct A {
  template <class T>
  operator auto() { return T(); } // { dg-warning "auto.*template" }
};
