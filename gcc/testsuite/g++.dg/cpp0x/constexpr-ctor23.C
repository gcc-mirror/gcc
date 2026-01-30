// Verify we diagnose and accept, as an extension, a non-empty constexpr
// constructor body in C++11 mode.
// PR c++/123845
// { dg-do compile { target c++11_only } }
// { dg-options "" }

constexpr int negate(int n) { return -n; }

struct A {
  int m;
  constexpr A() : m(42) {
    ++m;
    m = negate(m);
  } // { dg-warning "does not have empty body \[-Wc++14-extensions\]" }
};
static_assert(A().m == -43, "");

template<class T>
struct B {
  int m;
  constexpr B() : m(42) {
    ++m;
    m = negate(m);
  } // { dg-warning "does not have empty body \[-Wc++14-extensions\]" }
};
static_assert(B<int>().m == -43, "");
