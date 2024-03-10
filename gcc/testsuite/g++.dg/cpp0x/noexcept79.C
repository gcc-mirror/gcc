// PR c++/110468
// { dg-do compile { target c++11 } }

template<int T>
struct variant {
  variant() noexcept(T > 0);
};

template<int N>
struct A {
  variant<N> m = {};
};

struct B {
  B(A<1>);
};

B b = {{}};
