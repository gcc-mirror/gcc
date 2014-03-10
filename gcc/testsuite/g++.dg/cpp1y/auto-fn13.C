// { dg-do compile { target c++1y } }

struct A {
  template <class T>
  operator auto() { return T(); } // { dg-warning "auto.*template" }
};
