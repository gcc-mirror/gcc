// PR c++/52282
// { dg-do run { target c++11 } }

template <typename T, T V>
struct W { static constexpr T value() { return V; } };

template <typename T, T V>
struct X { typedef T type; static constexpr type value() { return V; } };

template <typename T, T V>
struct Y { using type = T; static constexpr type value() { return V; } };

template <typename T, T V>
struct Z { static constexpr decltype(V) value() { return V; } };

template <typename T, T V>
struct W_ { static constexpr T value = V; };

template <typename T, T V>
struct X_ { typedef T type; static constexpr type value = V; };

template <typename T, T V>
struct Y_ { using type = T; static constexpr type value = V; };

template <typename T, T V>
struct Z_ { static constexpr decltype(V) value = V; };


static_assert(W<int, 10>::value() == 10, "oops");
static_assert(X<int, 10>::value() == 10, "oops");
static_assert(Y<int, 10>::value() == 10, "oops");
static_assert(Z<int, 10>::value() == 10, "oops");
static_assert(W_<int, 10>::value == 10, "oops");
static_assert(X_<int, 10>::value == 10, "oops");
static_assert(Y_<int, 10>::value == 10, "oops");
static_assert(Z_<int, 10>::value == 10, "oops");

extern constexpr int a = 10;
static_assert(*W<const int*, &a>::value() == 10, "oops");
static_assert(*X<const int*, &a>::value() == 10, "oops");
static_assert(*Y<const int*, &a>::value() == 10, "oops");
static_assert(*Z<const int*, &a>::value() == 10, "oops");	// ICE
static_assert(*W_<const int*, &a>::value == 10, "oops");
static_assert(*X_<const int*, &a>::value == 10, "oops");
static_assert(*Y_<const int*, &a>::value == 10, "oops");
static_assert(*Z_<const int*, &a>::value == 10, "oops");	// ICE

template <int V> constexpr int b() { return V; }
static_assert((W<int(*)(), &b<10>>::value())() == 10, "oops");
static_assert((X<int(*)(), &b<10>>::value())() == 10, "oops");	// incorrect evaluation
static_assert((Y<int(*)(), &b<10>>::value())() == 10, "oops");	// incorrect evaluation
static_assert((Z<int(*)(), &b<10>>::value())() == 10, "oops");	// ICE
static_assert(W_<int(*)(), &b<10>>::value() == 10, "oops");
static_assert(X_<int(*)(), &b<10>>::value() == 10, "oops");
static_assert(Y_<int(*)(), &b<10>>::value() == 10, "oops");
static_assert(Z_<int(*)(), &b<10>>::value() == 10, "oops");	// ICE

constexpr struct C {
    constexpr int c1() const { return 10; }
    static constexpr int c2() { return 10; }
} c;

static_assert((c.*W<int(C::*)()const, &C::c1>::value())() == 10, "oops");
static_assert((c.*X<int(C::*)()const, &C::c1>::value())() == 10, "oops");
static_assert((c.*Y<int(C::*)()const, &C::c1>::value())() == 10, "oops");
static_assert((c.*Z<int(C::*)()const, &C::c1>::value())() == 10, "oops");
static_assert((c.*W_<int(C::*)()const, &C::c1>::value)() == 10, "oops");	// incorrect evaluation
static_assert((c.*X_<int(C::*)()const, &C::c1>::value)() == 10, "oops");	// incorrect evaluation
static_assert((c.*Y_<int(C::*)()const, &C::c1>::value)() == 10, "oops");	// incorrect evaluation
static_assert((c.*Z_<int(C::*)()const, &C::c1>::value)() == 10, "oops");	// incorrect evaluation

static_assert((W<int(*)(), &C::c2>::value())() == 10, "oops");
static_assert((X<int(*)(), &C::c2>::value())() == 10, "oops");	// incorrect evaluation
static_assert((Y<int(*)(), &C::c2>::value())() == 10, "oops");	// incorrect evaluation
static_assert((Z<int(*)(), &C::c2>::value())() == 10, "oops");	// ICE
static_assert(W_<int(*)(), &C::c2>::value() == 10, "oops");
static_assert(X_<int(*)(), &C::c2>::value() == 10, "oops");
static_assert(Y_<int(*)(), &C::c2>::value() == 10, "oops");
static_assert(Z_<int(*)(), &C::c2>::value() == 10, "oops");	// ICE


#include <assert.h>

template <typename T, T V>
constexpr typename X_<T, V>::type X_<T, V>::value;

int main() {
  C c;

  // correctly evaluates inside method scope
  int t1 = X<int(*)(), &b<10>>::value()();
  int t2 = (c.*X_<int(C::*)()const, &C::c1>::value)();
  int t3 = X<int(*)(), &C::c2>::value()();

  assert(t1 == 10);
  assert(t2 == 10);
  assert(t3 == 10);
  return 0;
}
