// PR c++/127109
// { dg-do compile { target c++14 } }
// { dg-options "" }

namespace std {
  using size_t = decltype (sizeof 0);
  template <typename> struct tuple_size;
  template <size_t, typename> struct tuple_element;
}

struct A {
  int a, b;
  bool c;
  constexpr explicit operator bool () const { return c; }
  template <std::size_t I>
  constexpr const int &get () const { return I == 0 ? a : b; }
};

template <>
struct std::tuple_size <A> { static constexpr int value = 2; };
template <std::size_t I>
struct std::tuple_element <I, A> { using type = const int; };

constexpr int
foo (int v)
{
  if (auto [a, b] = A { v, v * 2, v != 0 })	// { dg-warning "structured bindings in conditions only available with" "" { target c++23_down } }
    return a + b;
  return -1;
}

static_assert (foo (1) == 3);
static_assert (foo (0) == -1);
