// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

template<bool, typename _Tp = void> struct enable_if { };
template<typename _Tp> struct enable_if<true, _Tp> { typedef _Tp type; };


template <char... c>
constexpr typename enable_if<sizeof...(c) == 2, int>::type operator""_t ()
{
  return 2;
}

template <char... c>
constexpr typename enable_if<sizeof...(c) == 1, int>::type operator""_t ()
{
  return 1;
}

template <char... c>
constexpr typename enable_if<sizeof...(c) >= 3, int>::type operator""_t ()
{
  return 100;
}

int operator""_t (long double)
{
  return 200;
}

int main ()
{
  assert (45_t == 2);
  assert (4_t == 1);
  assert (100000_t == 100);
  assert (200.0_t == 200);
}
