// { dg-do compile { target c++17 } }

template<class T, class D = int>
struct S { T t; };
template<class U>
S(U) -> S<typename U::type>;

struct A {
  using type = short;
  operator type();
};
S s{A()};			// OK
S x(A());			// { dg-error "return type" }
