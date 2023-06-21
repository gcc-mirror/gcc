// PR c++/109160
// { dg-do compile { target c++20 } }

template<class T, bool B>
concept C = B;

template<int> struct X { };

template<bool B>
struct A {
  template<C<B> auto V> static void f();
  template<C<B> auto V> static void g(X<V>);
  template<C<B> auto V> static inline int value;
  template<C<B> auto V> struct D { };
};

int main() {
  A<true>::f<0>();
  A<false>::f<0>(); // { dg-error "no match|constraints" }

  A<true>::g(X<0>{});
  A<false>::g(X<0>{}); // { dg-error "no match|constraints" }

  bool v1 = A<true>::value<0>;
  bool v2 = A<false>::value<0>;  // { dg-error "constraints" }

  A<true>::D<0> d1;
  A<false>::D<0> d2; // { dg-error "constraints" }
}
