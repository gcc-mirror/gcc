// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct A { };

struct S {
  int m(this S) { return 42; }
  int n() const { return 42; }
  enum { FOO } e;
  A a;
};

auto p = &[: ^^S::m :];
auto q = &[: ^^S::m :];

auto rm = [: ^^S::m :];	 // { dg-error "cannot implicitly reference a class member .int S::m\\(this S\\). through a splice" }
auto rn = [: ^^S::n :];  // { dg-error "cannot implicitly reference a class member .int S::n\\(\\) const. through a splice" }
auto re = [: ^^S::e :];  // { dg-error "cannot implicitly reference a class member .S::e. through a splice" }
auto ra = [: ^^S::a :];  // { dg-error "cannot implicitly reference a class member .S::a. through a splice" }
