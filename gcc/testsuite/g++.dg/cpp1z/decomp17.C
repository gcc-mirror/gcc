// { dg-do compile { target c++17 } }

#include <tuple>

template <typename, typename> struct same_type;
template <typename T> struct same_type<T, T> {};

int main() {
  int i;
  std::tuple<int,int&,int&&> tuple = { 1, i, 1 };
  auto &[v, r, rr] = tuple;
  same_type<decltype(v), int>{};
  same_type<decltype(r), int&>{};
  same_type<decltype(rr), int&&>{};
}
