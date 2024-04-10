// PR c++/92687
// { dg-do compile { target c++11 } }
// { dg-options "" }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct A {
  int i;
  template <int I> int& get() { return i; }
};

template<> struct std::tuple_size<A> { static const int value = 2; };
template<int I> struct std::tuple_element<I,A> { using type = int; };

template<typename T>
struct is_reference {
  static const bool value = false;
};

template<typename T>
struct is_reference<T&>
{
  static const bool value = true;
};

template<typename T>
struct is_reference<T&&>
{
  static const bool value = true;
};

template<int N>
void
foo ()
{
  auto [x, y] = A {};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  static_assert (!is_reference<decltype (x)>::value, "");
}

void
bar ()
{
  auto [x, y] = A {};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  static_assert (!is_reference<decltype (x)>::value, "");
}

template<typename T>
void
baz ()
{
  auto [x, y] = T {};	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  static_assert (!is_reference<decltype (x)>::value, "");
}

void
qux ()
{
  foo<0> ();
  baz<A> ();
}
