// P1061R10 - Structured Bindings can introduce a Pack
// { dg-do compile { target c++11 } }
// { dg-options "" }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}
struct T {
  int a[3];
  template <int I>
  int &get () { return a[2 - I]; }
};
template <>
struct std::tuple_size<T> { static constexpr int value = 3; };
template <int N>
struct std::tuple_element<N, T> { typedef int type; };

template <int N>
inline void
foo ()
{
  static T t = { { N, N + 1, N + 2 } };
  static auto [ta, ...tb, tc] = t;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-2 }
}

template <int N>
inline void
bar ()
{
  thread_local T t = { { N, N + 1, N + 2 } };
  thread_local auto [...ta] = t;	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-warning "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-2 }
}

int
main ()
{
  foo <0> ();
  bar <0> ();
}
