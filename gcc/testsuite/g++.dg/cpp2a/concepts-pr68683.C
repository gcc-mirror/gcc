// PR c++/68683
// { dg-do compile { target c++20 } }

template <typename, typename>
struct is_same {
  static constexpr bool value = true;
};

template <typename T, typename U>
concept Same = is_same<T, U>::value;

template <typename T>
concept Integral = requires {
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
