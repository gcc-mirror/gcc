// PR c++/71093
// { dg-do compile { target c++14 } }

constexpr int f (const int *p)
{
  typedef int T;
  p->~T ();   // { dg-error "destroying" }
  return *p;
}

constexpr int i = 0;
constexpr int j = f (&i);


template <typename T>
constexpr bool test_access() {
  T x {};
  x.~T();
  T y = x;  // { dg-error "lifetime" }
  return true;
}

template <typename T>
constexpr bool test_modification() {
  T x {};
  x.~T();
  x = T();  // { dg-error "lifetime" }
  return true;
}

template <typename T>
constexpr bool test_scope() {
  {
    T x {};
    x.~T();
  }  // { dg-error "destroying" }
  return true;
}

template <typename T>
constexpr bool test_destroy_temp() {
  T{}.~T();  // { dg-error "destroying" }
  return true;
}

template <typename T>
constexpr bool test_parameter(T t) {
  // note: error message occurs at point of call
  t.~T();
  return true;
}

template <typename T>
constexpr void test_bindings_impl(int n) {
  if (n == 0) return;
  T a {};
  if (n == 1) return;
  T b {};
}

template <typename T>
constexpr bool test_bindings() {
  test_bindings_impl<T>(1);
  test_bindings_impl<T>(0);
  test_bindings_impl<T>(2);
  return true;
}

constexpr bool i1 = test_access<int>();        // { dg-message "in .constexpr." }
constexpr bool i2 = test_modification<int>();  // { dg-message "in .constexpr." }
constexpr bool i3 = test_scope<int>();         // { dg-message "in .constexpr." }
constexpr bool i4 = test_destroy_temp<int>();  // { dg-message "in .constexpr." "" { xfail *-*-* } }
constexpr bool i5 = test_parameter(int{});     // { dg-error "destroying" }
constexpr bool i6 = test_bindings<int>();

struct Trivial { int x; };
constexpr bool t1 = test_access<Trivial>();        // { dg-message "in .constexpr." }
constexpr bool t2 = test_modification<Trivial>();  // { dg-message "in .constexpr." }
constexpr bool t3 = test_scope<Trivial>();         // { dg-message "in .constexpr." }
constexpr bool t4 = test_destroy_temp<Trivial>();  // { dg-message "in .constexpr." }
constexpr bool t5 = test_parameter(Trivial{});     // { dg-error "destroying" }
constexpr bool t6 = test_bindings<Trivial>();

#if __cplusplus >= 202002L
struct NonTrivial { int x; constexpr ~NonTrivial() {} };  // { dg-error "destroying" "" { target c++20 } }
constexpr bool n1 = test_access<NonTrivial>();        // { dg-message "in .constexpr." "" { target c++20 } }
constexpr bool n2 = test_modification<NonTrivial>();  // { dg-message "in .constexpr." "" { target c++20 } }
constexpr bool n3 = test_scope<NonTrivial>();         // { dg-message "in .constexpr." "" { target c++20 } }
constexpr bool n4 = test_destroy_temp<NonTrivial>();  // { dg-message "in .constexpr." "" { target c++20 } }
constexpr bool n5 = test_parameter(NonTrivial{});     // { dg-message "in .constexpr." "" { target c++20 } }
constexpr bool n6 = test_bindings<NonTrivial>();
#endif

