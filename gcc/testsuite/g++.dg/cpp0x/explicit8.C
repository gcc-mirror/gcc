// PR c++/60686
// { dg-do compile { target c++11 } }

struct A {
  explicit operator int() const;
};

explicit inline A::operator int() const { return 1; }  // { dg-error "1:'explicit' outside class declaration" }

struct B {
  explicit void f();  // { dg-error "3:only declarations of constructors and conversion operators can be 'explicit'" }
};

explicit void B::f() { }  // { dg-error "1:'explicit' outside class declaration" }

struct C {
  explicit C(int);
};

struct D {
  explicit friend C::C(int);  // { dg-error "3:'explicit' in friend declaration" }
};
