// PR c++/78358
// { dg-do run }
// { dg-options -std=c++17 }

#include <tuple>

template <typename, typename> struct same_type;
template <typename T> struct same_type<T, T> {};

int main() {
  std::tuple tuple = { 1, 'a', 2.3, true };
  auto[i, c, d, b] = tuple;
  same_type<std::tuple_element<0, decltype(tuple)>::type, decltype(i)>{};
  same_type<decltype(i), int>{};
  same_type<decltype(c), char>{};
  same_type<decltype(d), double>{};
  same_type<decltype(b), bool>{};
  if (i != 1 || c != 'a' || d != 2.3 || b != true)
    __builtin_abort ();
}
