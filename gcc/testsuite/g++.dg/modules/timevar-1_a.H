// PR c++/115165
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template <typename> struct A { virtual ~A(); };
struct B : A<char> {};
struct C : B { C() {} };

class D { C c; };
void f(D);

struct X {
  friend void f(X);
};
