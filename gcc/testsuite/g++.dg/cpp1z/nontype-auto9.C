// PR c++/79556
// { dg-options -std=c++1z }

template <auto> struct A;
template <auto...> struct B;
template <int N, auto Dim, auto... Dims> struct B<N, Dim, Dims...> {
  static auto a = A<B<Dims...>::value>::value;
};
