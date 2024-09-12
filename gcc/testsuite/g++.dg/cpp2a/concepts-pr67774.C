// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <type_traits>
#include <utility>
#include <iostream>

template <class X> concept cpt_RealScalar =
  std::is_floating_point<X>::value;

namespace detail {
template <class, class> constexpr bool k_evaluator_impl = false;

template <std::size_t... Indexes, class E>
constexpr bool k_evaluator_impl<std::index_sequence<Indexes...>, E> = true;
}

template <class X, std::size_t K> concept cpt_KEvaluator =
  detail::k_evaluator_impl<std::make_index_sequence<K>, X>;

int main() {
  auto f = [](int, int, int) -> double { return 3; };
  std::cout << cpt_KEvaluator<decltype(f)> << '\n'; // { dg-error "wrong number of template arguments" }
  return 0;
}
