// PR c++/51723
// { dg-options -std=c++0x }

template <int... V>
struct A
{
  static constexpr int a[sizeof...(V)] = { V... };
};

template <int... V> constexpr int A<V...>::a[];

struct B
{
  const int* const b;

  template <unsigned int N>
  constexpr B(const int(&b)[N])
  : b(b)
  { }

  template <int... V>
  constexpr B(A<V...>)
  : B(A<V...>::a)
  { }
};
