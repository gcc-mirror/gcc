// PR c++/123667
// { dg-do compile { target c++14 } }
// { dg-options "" }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}

struct A {
  int i;
  template <int I> int &get () { return i; }
};

template <> struct std::tuple_size <A> { static const int value = 2; };
template <int I> struct std::tuple_element <I,A> { using type = int; };

int
main ()
{
  auto [ x, y ] = A { 0 };	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  [] (auto t) { using z = decltype (x); } (1);
}
