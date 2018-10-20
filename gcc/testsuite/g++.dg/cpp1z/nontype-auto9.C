// PR c++/79556
// { dg-do compile { target c++17 } }

template <auto> struct A;
template <auto...> struct B;
template <int N, auto Dim, auto... Dims> struct B<N, Dim, Dims...> {
  static auto a = A<B<Dims...>::value>::value;
};
