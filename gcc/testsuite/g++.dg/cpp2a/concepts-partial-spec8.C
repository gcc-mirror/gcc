// { dg-do compile { target c++20 } }

template<int M, int N>
concept equal = M == N;

template<int M>
struct traits
{
  template<int N> requires equal<M, N>
    struct foo {};

  template<int N> requires equal<M, N> && (M >= 0) // { dg-bogus "not more constrained" }
    struct foo<N> {};
};
