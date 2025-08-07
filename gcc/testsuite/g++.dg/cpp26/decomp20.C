// P1061R10 - Structured Bindings can introduce a Pack
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S {
  explicit operator bool () const noexcept { return true; }
};
namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}
int x;
struct T {
  template <int I>
  int &get () { return x; }
  explicit operator bool () const noexcept { return false; }
};
template <>
struct std::tuple_size<T> { static constexpr int value = 0; };
template <int N>
struct std::tuple_element<N, T> { typedef int type; };

template <int N>
void
foo ()
{
  int a[0] = {};
  auto [...aa] = a;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (aa) == 0, "");
  S s = {};
  auto [...sa] = s;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (sa) == 0, "");
  T t = {};
  auto [...ta] = t;		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (ta) == 0, "");
  if (auto [...sb] = s)		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    static_assert (sizeof... (sb) == 0, "");
  else
    __builtin_abort ();
  if (auto [...tb] = t)		// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    __builtin_abort ();
  else
    static_assert (sizeof... (tb) == 0, "");
}

int
main ()
{
  foo <0> ();
}
