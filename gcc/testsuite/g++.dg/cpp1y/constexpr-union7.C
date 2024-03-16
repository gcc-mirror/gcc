// { dg-do compile { target c++14 } }

// this type is not value-initialisable
struct S { const int a; int b; };

union U1 { int k; S s; };
constexpr int test1() {
  U1 u {};
  return u.s.b;  // { dg-error "accessing .U1::s. member instead of initialized .U1::k. member" }
}
constexpr int x = test1();

union U2 { int :0; static int s; void foo(); int k; };
constexpr int test2() {
  U2 u {};  // should skip zero-width bitfields, static members, and functions
  return u.k;
}
static_assert(test2() == 0, "");
