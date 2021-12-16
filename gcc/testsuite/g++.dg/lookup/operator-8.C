// Verify phase 1 lookup works properly for rewritten non-dependent conditional
// operator expressions.

// This test currently fails due to build_min_non_dep_op_overload not knowing
// how to handle rewritten operator expressions; see the FIXME in build_new_op.

// { dg-do compile { target c++20 } }

#include <compare>

struct A {
  bool operator==(int);
  std::strong_ordering operator<=>(int);
};

template<class T>
void f() {
  A a;
  (void)(a != 0, 0 != a); // { dg-bogus "deleted" "" { xfail *-*-* } }
  (void)(a < 0, 0 < a);   // { dg-bogus "deleted" "" { xfail *-*-* } }
  (void)(a <= 0, 0 <= a); // { dg-bogus "deleted" "" { xfail *-*-* } }
  (void)(a > 0, 0 > a);   // { dg-bogus "deleted" "" { xfail *-*-* } }
  (void)(a >= 0, 0 >= a); // { dg-bogus "deleted" "" { xfail *-*-* } }
}

// These later-declared namespace-scope overloads shouldn't be considered
// when instantiating f<int>.
bool operator!=(A, int) = delete;
bool operator<(A, int) = delete;
bool operator<=(A, int) = delete;
bool operator>(A, int) = delete;
bool operator>=(A, int) = delete;

template void f<int>();
