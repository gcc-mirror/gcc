// PR c++/115425
// { dg-do compile { target c++20 } }

using size_t = decltype(sizeof(0));

template <int... I>
struct X {};

template<int... Is>
void foo(X<Is...>);

template<auto>
struct S;

template<class T>
auto test() {
  constexpr static auto x = foo<X<__integer_pack (0)...>>(); // { dg-error "no matching function" }
  return []<size_t... Is>(X<Is...>) {
    (typename S<x[Is]>::type{}, ...);
  }(X<__integer_pack (0)...>{});
}

int main() {
  test<int>();
}
