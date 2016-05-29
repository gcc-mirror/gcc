// PR c++/71105
// { dg-do compile { target c++14 } }

template <typename T> T declval();
template <typename, typename> struct is_same
{ static constexpr bool value = false; };
template <typename T> struct is_same<T, T>
{ static constexpr bool value = true; };

template <class F>
struct indirected : F {
  indirected(F f) : F(f) {}
  template <class I>
  auto operator()(I i) -> decltype(declval<F&>()(*i)) {
    return static_cast<F&>(*this)(*i);
  }
};

int main() {
  auto f = [=](auto i) { return i + i; };
  auto i = indirected<decltype(f)>{f};
  static_assert(is_same<decltype(i(declval<int*>())), int>::value, "");
}
