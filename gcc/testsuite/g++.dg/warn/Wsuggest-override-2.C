// PR c++/78010
// { dg-options "-std=c++11 -Wsuggest-override" }

struct A {
  virtual void f();
};
struct B : A {
  void f() final; // { dg-bogus "can be marked override" }
};
