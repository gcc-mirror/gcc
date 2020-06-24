// Pack expansion testcases from P2082R1
// { dg-do compile { target c++20 } }

template<typename U, typename... T>
struct C2 : T... {
  U a;
  static constexpr int len = sizeof...(T);
};
C2 c2 = {
	 []{ return 1; },
};
static_assert (c2.len == 0);

template <typename... T>
struct Types {};
template <typename... T>
struct F : Types<T...>, T... {};
struct X {};
struct Y {};
struct Z {};
struct W { operator Y(); };
F f1 = {Types<X, Y, Z>{}, {}, {}}; // OK, F<X, Y, Z> deduced
F f2 = {Types<X, Y, Z>{}, X{}, Y{}}; // OK, F<X, Y, Z> deduced
F f3 = {Types<X, Y, Z>{}, X{}, W{}}; // { dg-error "" } conflicting types deduced; operator Y not considered
