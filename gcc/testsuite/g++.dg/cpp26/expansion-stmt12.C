// C++26 P1306R5 - Expansion statements
// { dg-do compile { target c++14 } }
// { dg-options "" }

constexpr int
foo (auto const &... x)				// { dg-warning "use of 'auto' in parameter declaration only available with" "" { target c++17_down } }
{
  int r = 0;
  template for (auto const &c : {x...})		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    r += c[0];
  return r;
}

constexpr int c1[] = { 1, 2, 3 };
constexpr int c2[] = { 4, 3, 2, 1 };
static_assert (foo (c1, c2) == 5, "");

template <typename T, unsigned long N>
struct array
{
  T e[N];
  constexpr T *begin () noexcept { return &e[0]; }
  constexpr const T *begin () const noexcept { return &e[0]; }
  constexpr T *end () noexcept { return &e[N]; }
  constexpr const T *end () const noexcept { return &e[N]; }
};

static constexpr array <int, 3> a { 1, 2, 3 };

constexpr int
bar ()
{
  int r = 0;
  template for (constexpr int s : a)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    r += sizeof (char[s]);
  return r;
}

static_assert (bar () == 6, "");

struct S { int i; short s; };

constexpr long
baz (S s)
{
  long r = 0;
  template for (auto x : s)			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      r += sizeof (x);
    }
  return r;
}

static_assert (baz (S {}) == sizeof (int) + sizeof (short), "");
