// C++26 P1306R5 - Expansion statements
// { dg-do run { target c++14 } }
// { dg-options "" }

template <typename T, typename U>
constexpr bool is_same_v = false;

template <typename T>
constexpr bool is_same_v<T, T> = true;

struct S { int a; long b; short c; };
struct T { long long a; unsigned b; signed char c; };
struct U { float a; double b; long double c; };
struct V { S l, m, n; T o; U p; };
constexpr S d = { 1, 2, 3 }, e = { 4, 5, 6 }, f = { 7, 8, 9 };
constexpr T j = { 10, 11, 12 };
U k = { 13.0f, 14.5, 15.5 }, m = { 7.0f, 7.0, 7.0 };
V l = { d, e, f, j, k };
struct A
{
  int x;
  constexpr explicit A (int v) : x(v) {}
  constexpr A &operator ++ () { ++x; return *this; }
  constexpr int operator * () const { return x; }
  constexpr bool operator != (const A &o) const { return x != o.x; }
  constexpr A operator + (int o) const { A r (x + o); return r; }
  constexpr int operator - (const A &o) const { return x - o.x; }
};
struct C
{
  int x, y, z;
  constexpr explicit C (int u, int v, int w) : x(u), y(v), z(w) {}
  constexpr C &operator ++ () { ++x; --y; ++z; return *this; }
  constexpr C operator * () const { return *this; }
  constexpr bool operator != (const C &o) const { return x != o.x || y != o.y || z != o.z; }
  constexpr C operator + (int o) const { C r (x + o, y - o, z + o); return r; }
  constexpr int operator - (const C &o) const { return x - o.x; }
};

namespace N
{
  struct B { constexpr B () {} };
  constexpr A begin (B &) { return A (0); }
  constexpr A end (B &) { return A (6); }
}

namespace O
{
  struct D { constexpr D () {} };
  constexpr C begin (D &) { return C (0, 42, 5); }
  constexpr C end (D &) { return C (6, 36, 11); }
}

long long
foo ()
{
  long long r = 0;
  template for (auto &g : { d, e, f, j, k })			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      r += g.a + g.b + g.c;
      decltype (g) s = g;
      r += sizeof (s);
    }
  return r;
}

int
bar ()
{
  int r = 0;
  template for (auto i : N::B {})				// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      r += i;
      static_assert (is_same_v <decltype (i), int>);
    }
  return r;
}

int
baz ()
{
  int a[] = { 2, 4, 6, 8, 10 };
  int r = 0, i = 0;
  template for (const int &w : a)				// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      if (&w != &a[i++])
	break;
      r += w;
      if (w == 6)
	continue;
      ++r;
    }
  return r;
}

long long
qux ()
{
  long long r = 0;
  template for (const auto &i : l)				// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      r += i.a * i.b * i.c;
      decltype (i.a) s = 0;
      decltype (i.c) t = 0;
      r += sizeof (s) + sizeof (t);
    }
  return r;
}

long long
corge ()
{
  long long r = 0;
  int z = 0;
  template for (const auto &[g, h, i] : { d, e, f, j, m, k, k })// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {								// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      ++z;
      if (z == 5)
	continue;
      ++r;
      if (z == 7)
	break;
      r += g + h + i;
      decltype (h) s = 0;
      r += sizeof (s) + sizeof (i);
    }
  return r;
}

int
garply ()
{
  int r = 0;
  template for (auto [g, h, i] : O::D {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    r += g + h + i;						// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  return r;
}

int
freddy ()
{
  S a[] = { { 2, 4, 6 }, { 8, 10, 12 }, { 14, 16, 18 } };
  int r = 0, i = 0;
  template for (const auto &[u, v, w] : a)			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {								// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
      if (&u != &a[i].a || &v != &a[i].b || &w != &a[i].c)
	break;
      ++i;
      r += u + v + w;
      if (w == 12)
	continue;
      ++r;
    }
  return r;
}

long long
quux ()
{
  long long r = 0;
  template for (auto [i, j, k] : l)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    r += i * j * k;				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  return r;
}

long long
boo ()
{
  long long r = 0;
  template for (auto g : { 1, 2U, 3LL, 4ULL })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      switch (g)
	{
	case 1:
	  r += 3;
	  break;
	case 2:
	  r += 5;
	  break;
	case 3:
	  r += 9;
	  break;
	case 4:
	  r += 13;
	  break;
	default:
	  __builtin_abort ();
	}
    }
  return r;
}

int
main ()
{
  if (foo () != 121 + 3 * sizeof (S) + sizeof (T) + sizeof (U))
    __builtin_abort ();
  if (bar () != 15)
    __builtin_abort ();
  if (baz () != 34)
    __builtin_abort ();
  if (qux () != (4871 + 3 * (sizeof (int) + sizeof (short))
		 + sizeof (long long) + sizeof (signed char)
		 + sizeof (float) + sizeof (long double)))
    __builtin_abort ();
  if (corge () != (127 + 3 * (sizeof (long) + sizeof (short))
		   + sizeof (unsigned) + sizeof (signed char)
		   + sizeof (double) + sizeof (long double)))
    __builtin_abort ();
  if (garply () != 297)
    __builtin_abort ();
  if (freddy () != 92)
    __builtin_abort ();
  if (quux () != 4871)
    __builtin_abort ();
  if (boo () != 30)
    __builtin_abort ();
}
