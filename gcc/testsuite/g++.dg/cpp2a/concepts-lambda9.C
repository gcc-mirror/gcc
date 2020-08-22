// PR c++/93698
// { dg-do compile { target concepts } }

#include <utility>

template <int N>
concept foo = []<std::size_t... Is>(std::index_sequence<Is...>) constexpr {
  return (Is + ...) > 10;
}(std::make_index_sequence<N>());

bool a = foo<7>;
