// PR c++/123676
// { dg-do compile { target c++20 } }

template<class...>
concept C = true;

template<class... Ts>
auto f() -> decltype(C<Ts...>) {
  return true;
}
