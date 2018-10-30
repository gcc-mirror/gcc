// P0892R2
// { dg-do compile }
// { dg-options "-std=c++2a" }

constexpr int fn0 () { return 0; }
constexpr int fn1 () { return 1; }

struct S {
  explicit(true) S(int);
  explicit(1 == 0) S(int, int);
  explicit(fn0()) S(int, int, int);
  explicit(fn1()) S(int, int, int, int);
};

struct X {
  static const bool value = true;
  static constexpr bool foo () { return 1; }
};

struct T {
  explicit(true ? 1 : throw 1) T(int);
  explicit(true || true ? 1 : throw 1) T(int, int);
  explicit(X::value) T(int, int, int);
  explicit(X::foo ()) T(int, int, int, int);
};

struct W {
  constexpr operator bool() { return true; };
};

struct W2 {
  constexpr operator bool() { return false; };
};

struct U {
  explicit(W()) U(int);
  explicit(W2()) U(int, int);
};

int
main ()
{
  S s1 = { 1 }; // { dg-error "converting" }
  S s1x{ 1 };
  S s2 = { 2, 3 };
  S s3 = { 4, 5, 6 };
  S s4 = { 7, 8, 9, 10 }; // { dg-error "converting" }
  S s4x{ 7, 8, 9, 10 };

  T t1 = { 1 }; // { dg-error "converting" }
  T t2 = { 1, 2 }; // { dg-error "converting" }
  T t3 = { 1, 2, 3 }; // { dg-error "converting" }
  T t4 = { 1, 2, 3, 4 }; // { dg-error "converting" }
  T t5{ 1 };
  T t6{ 1, 2 };
  T t7{ 1, 2, 3 };
  T t8{ 1, 2, 3, 4 };

  U u1 = { 1 }; // { dg-error "converting" }
  U u2{ 1 };
  U u3 = { 1, 2 };
  U u4 { 1, 2 };
}
