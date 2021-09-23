// PR c++/99664
// { dg-do compile { target c++11 } }

struct Res { };

struct A {
  virtual Res &&f();
  virtual Res &g();
};

struct B : A {
  Res &f() override; // { dg-error "return type" }
  Res &&g() override; // { dg-error "return type" }
};
