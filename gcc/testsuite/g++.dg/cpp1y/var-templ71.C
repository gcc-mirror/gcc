// PR c++/108550
// { dg-do compile { target c++14 } }

template<class T, T T1>
struct integral_constant
{
  static constexpr T value = T1;
};

template <typename T>
struct S {
  template <typename U, typename V>
  static constexpr void foo(V) { }

  constexpr bool bar () const { foo<int>(10); return false; }
};

template <class Tp>
constexpr auto is_pointer_v = S<Tp>{}.bar();

template <class Tp, int = 0>
integral_constant<bool, is_pointer_v<int>> Wrap1();

int main() {
  static_assert(!decltype(Wrap1<int>())::value, "");
}
