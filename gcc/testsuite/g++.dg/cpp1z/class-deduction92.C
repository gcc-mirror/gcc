// PR c++/79501
// { dg-do compile { target c++17 } }

template<auto V>
struct X {
  template<class T, auto>
  struct B { T t; };

  template<class T> B(T, decltype(V)=V) -> B<const T, V>;

  auto foo() { return B{V}; }
};

X<42> x;
using type = decltype(x.foo());
using type = decltype(decltype(x)::B{42});
using type = X<42>::B<const int, 42>;
