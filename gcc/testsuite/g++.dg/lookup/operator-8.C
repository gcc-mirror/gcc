// Verify phase 1 lookup works properly for rewritten non-dependent conditional
// operator expressions.

// { dg-do compile { target c++20 } }

#include <compare>

struct A {
  bool operator==(int);
  std::strong_ordering operator<=>(int);
};

template<class T>
void f() {
  A a;
  (void)(a != 0);
  (void)(0 != a);
  (void)(a < 0);
  (void)(0 < a);
  (void)(a <= 0);
  (void)(0 <= a);
  (void)(a > 0);
  (void)(0 > a);
  (void)(a >= 0);
  (void)(0 >= a);
}

// These later-declared namespace-scope overloads shouldn't be considered
// when instantiating f<int>.
bool operator!=(A, int) = delete;
bool operator<(A, int) = delete;
bool operator<=(A, int) = delete;
bool operator>(A, int) = delete;
bool operator>=(A, int) = delete;

bool operator!=(int, A) = delete;
bool operator<(int, A) = delete;
bool operator<=(int, A) = delete;
bool operator>(int, A) = delete;
bool operator>=(int, A) = delete;

template void f<int>();
