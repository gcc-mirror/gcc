// { dg-do compile { target c++14 } }
// { dg-options "" }

struct A {
  template <class T>
  operator auto() { return T(); } // { dg-warning "auto.*template" }
};
