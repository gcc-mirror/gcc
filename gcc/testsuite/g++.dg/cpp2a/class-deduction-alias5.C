// PR c++/99180
// { dg-do compile { target c++17 } }

template <bool, typename... Ts>
struct A {
  A(Ts...) {}
};

template <typename... Ts>
using B = A<false, Ts...>;

template <typename... Ts>
A(Ts...) -> A<true, Ts...>;

int main() {
  B{};				// { dg-error "alias" "" { target c++17_down } }
  return 0;
}
