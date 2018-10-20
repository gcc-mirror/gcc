// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <typename, typename>
struct is_same {
  static constexpr bool value = true;
};

template <typename T, typename U>
concept bool Same = is_same<T, U>::value;

template <typename T>
concept bool Integral = requires {
  { T () } -> Same<typename T::value_type>;
};

struct A {
  using value_type = bool;
};

int main () {
  Integral<A>;
  Integral<A>;
  return 0;
}
