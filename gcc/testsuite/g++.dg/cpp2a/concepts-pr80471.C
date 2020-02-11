// PR c++/80471
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

template <class, class>
constexpr bool is_same = false;
template <class T>
constexpr bool is_same<T, T> = true;

template<class T>
decltype(auto) f(T&& a){return a;}

decltype(auto) g(auto&& a){return a;}

auto z = [](auto&& a) -> decltype(auto) { return a; };

int main()
{
  int i;
  static_assert(is_same< decltype(f(i)), int& >, "");
  static_assert(is_same< decltype(g(i)), int& >, "");
  static_assert(is_same< decltype(z(i)), int& >, "");
}
