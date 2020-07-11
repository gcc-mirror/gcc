// PR c++/92150
// { dg-do compile { target c++20 } }

struct X {
  int value;
  // auto operator==(const X&) = default;
};

template<typename T, X N>
struct b;

template<typename T>
inline constexpr bool is_b = false;

template<typename T, X N>
inline constexpr bool is_b<b<T, N>> = true;

using my_b = b<int, X{1}>;
static_assert(is_b<my_b>);
