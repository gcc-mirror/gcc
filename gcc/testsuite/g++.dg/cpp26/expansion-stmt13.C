// C++26 P1306R5 - Expansion statements
// { dg-do run { target c++11 } }
// { dg-options "" }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}

struct S { int s; };
constexpr S c[] = { { 3 }, { 4 }, { 5 }, { 6 }, { 7 } };
struct U {
  constexpr const S *begin () const { return &c[0]; }
  constexpr const S *end () const { return &c[s]; }
  int s;
};
constexpr U u1 = { 3 }, u2 = { 0 };

struct V {
  int i, j;
  template <int I> int &get () { return i; }
};

template<> struct std::tuple_size <V> { static const int value = 2; };
template<int I> struct std::tuple_element <I, V> { using type = int; };

struct W {
  int w;
  W (int x) : w (x) {}
  ~W () {}
};

struct X {
  V i, j;
  template <int I> V &get () { return j; }
};

template<> struct std::tuple_size <X> { static const int value = 3; };
template<int I> struct std::tuple_element <I, X> { using type = V; };

long long
foo ()
{
  long long r = 0;
  template for (auto h = 2; constexpr auto g : u1)	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    r += g.s + h;
  template for (long long h = ++r; auto g : u2)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    __builtin_abort ();
  return r;
}

long long
bar ()
{
  long long r = 0;
  template for (W w { 42 }; auto i : V { 42, 10 })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    r += i + (w.w == 42);
  return r;
}

long long
baz ()
{
  long long r = 0;
  template for (constexpr auto x = 5; auto [ i, j ] : X { { 5, 6 }, { 7, 8 } })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    r += i + j + (x == 5);							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  return r;
}

V &&
qux (V &&x) noexcept
{
  return static_cast<V &&> (x);
}

long long
freddy ()
{
  long long r = 0;
  V a { 1, 2 }, b { 3, 4 };
  template for (auto i : { qux (static_cast<V &&> (a)), qux (static_cast<V &&> (b)) })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    r += i.i + i.j;
  return r;
}

int
main ()
{
  if (foo () != 19)
    __builtin_abort ();
  if (bar () != 86)
    __builtin_abort ();
  if (baz () != 45)
    __builtin_abort ();
  if (freddy () != 10)
    __builtin_abort ();
}
