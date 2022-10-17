// P1169R4 - static operator()
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <typename T>
struct S
{
  static constexpr bool operator () (T const &x, T const &y) { return x < y; }; // { dg-warning "may be a static member function only with" "" { target c++20_down } }
  using P = bool (*) (T const &, T const &);
  operator P () const { return operator (); }
};

static_assert (S<int> {} (1, 2), "");

template <typename T>
void
bar (T &x)
{
  x (1, 2);
}

void
foo ()
{
#if __cpp_constexpr >= 201603L
  auto a = [](int x, int y) static constexpr { return x + y; };			// { dg-warning "'static' only valid in lambda with" "" { target { c++17 && c++20_down } } }
  static_assert (a (1, 2) == 3, "");
  bar (*a);
#endif
  auto b = []() static { return 1; };						// { dg-warning "'static' only valid in lambda with" "" { target c++20_down } }
  b ();
  auto c = [](int x, int y) static { return x + y; };				// { dg-warning "'static' only valid in lambda with" "" { target c++20_down } }
  c (1, 2);
  bar (*c);
#if __cpp_generic_lambdas >= 201707L
  auto d = []<typename T, typename U>(T x, U y) static { return x + y; };	// { dg-warning "'static' only valid in lambda with" "" { target c++20_only } }
  d (1, 2L);
#endif
  S<long> s;
  s(1L, 2L);
}
