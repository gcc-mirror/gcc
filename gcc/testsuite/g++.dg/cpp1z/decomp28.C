// PR c++/80370
// { dg-do compile { target c++14 } }
// { dg-options "" }

namespace std {
  template <typename> struct tuple_size;
  template <long, typename> struct tuple_element;
  template <typename...> struct tuple {};
  template <typename... T> struct tuple_size<tuple<T...>> { static constexpr int value = 1; };
  template <typename T, typename... U> struct tuple_element<0, tuple<T, U...>> { typedef T type; };
  template <int, typename... T> int& get (tuple<T...>);
}

template <int N>
void
foo (std::tuple<int> b)
{
  auto [c] = b;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
}

template <typename T>
void
bar (std::tuple<T> b)
{
  auto [c] = b;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
}

void
baz (std::tuple<int> b)
{
  foo <5> (b);
  bar (b);
}

int
main ()
{
  [](auto) { [](std::tuple<int> b) { auto[c] = b; }; } (0); // { dg-warning "structured bindings only available with" "" { target c++14_down } }
}
