// PR c++/60463
// PR c++/60755
// { dg-do compile { target c++11 } }
struct S {
  void f();
  void g() const {
    [=] { f(); } (); // { dg-error "no match|qualifiers" }
// { dg-warning "implicit capture" "" { target c++2a } .-1 }
  }
};
