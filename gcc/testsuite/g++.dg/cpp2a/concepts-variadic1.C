// PR c++/96531
// { dg-do compile { target c++20 } }

template<typename T>
concept is_bool = __is_same(bool, T);

template <typename... Ts>
concept C = requires {
  requires (is_bool<Ts> || ...);
};

template <bool... Bs>
concept D = requires {
  requires (Bs || ...);
};

template <typename... Ts>
requires C<Ts...>
void bar() {}

template <bool... Bs>
requires D<Bs...>
void baz() {}

int main() {
  bar<int, char, bool>();
  baz<false, true, false>();
}
