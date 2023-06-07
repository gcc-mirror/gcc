// P0892R2
// { dg-do compile }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.
// { dg-options "-std=c++2a" }

constexpr int fn0 () { return 0; }
constexpr int fn1 () { return 1; }

struct S0 {
  explicit(false) operator int();
  explicit(1 == 0) operator double();
  explicit(fn0()) operator char();
};

struct S1 {
  explicit(true) operator int();
  explicit(1 == 1) operator double();
  explicit(fn1()) operator char();
};

struct X {
  static const bool value = true;
  static constexpr bool foo () { return 1; }
};

struct T {
  explicit(true ? 1 : throw 1) operator int();
  explicit(true || true ? 1 : throw 1) operator double();
  explicit(X::value) operator char();
  explicit(X::foo ()) operator long();
};

struct W {
  constexpr operator bool() { return true; };
};

struct W2 {
  constexpr operator bool() { return false; };
};

struct U1 {
  explicit(W()) operator int();
};

struct U2 {
  explicit(W2()) operator int();
};

int
main ()
{
  S0 s0;
  S1 s1;
  int i0 = s0;
  int i1 = s1; // { dg-error "cannot convert" }
  double d0 = s0;
  double d1 = s1; // { dg-error "cannot convert" }
  char c0 = s0;
  char c1 = s1; // { dg-error "cannot convert" }

  T t;
  int i2 = t; // { dg-error "cannot convert" }
  double d2 = t; // { dg-error "cannot convert" }
  char c2 = t; // { dg-error "cannot convert" }
  long l1 = t; // { dg-error "cannot convert" }

  U1 u1;
  int i3 = u1; // { dg-error "cannot convert" }

  U2 u2;
  int i4 = u2;
}
