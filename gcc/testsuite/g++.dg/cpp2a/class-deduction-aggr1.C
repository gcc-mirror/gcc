// Testcase from P1816R0
// { dg-do compile { target c++20 } }

template <typename T>
struct S {
  T x;
  T y;
};

S s = { 1, 2 };

template <typename T>
struct C {
  S<T> s;
  T t;
};
template <typename T>
struct D {
  S<int> s;
  T t;
};
C c1 = {1, 2};			// { dg-error "" "deduction failed" }
C c2 = {1, 2, 3};		// { dg-error "" "deduction failed" }
C c3 = {{1u, 2u}, 3};		// { dg-bogus "" "OK, C<int> deduced" }
D d1 = {1, 2};			// { dg-error "" "deduction failed" }
D d2 = {1, 2, 3};	 // { dg-bogus "" "OK, braces elided, D<int> deduced" }
template <typename T>
struct I {
  using type = T;
};
template <typename T>
struct E {
  typename I<T>::type i;
  T t;
};
E e1 = {1, 2};			// { dg-bogus "" "OK, E<int> deduced" }
