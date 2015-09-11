// PR c++/65080
// { dg-do compile { target c++11 } }

template <typename T>
static constexpr T xxx(){ return T(); }

template <typename T>
struct foo {
  using type = T(*)();
  static constexpr type value[1] = {&xxx<T>};
};

template <typename T>
constexpr typename foo<T>::type foo<T>::value[1];

int main() {
  constexpr int x = foo<int>::value[0]();
}
