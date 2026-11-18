// Test constexpr modifications from a checked contract.
// { dg-additional-options -fcontracts }
// { dg-do compile { target c++14 } }

struct A
{
  mutable int i;
  constexpr operator bool() const { i = 42; return true; }
};

constexpr int f(A& a) pre (a) { return a.i; }

constexpr int g() {
  A a { };
  return f(a);
}
static_assert (g() == 42, "");
