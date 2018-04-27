// LWG2296 - addressof should be constexpr
// { dg-do run { target c++11 } }

template <typename T>
constexpr inline T *
addressof (T &x) noexcept
{
  return __builtin_addressof (x);
}

int i;
static_assert (__builtin_addressof (i) == &i, "");
static_assert (addressof (i) == &i, "");

constexpr int &j = i;
static_assert (__builtin_addressof (j) == &i, "");
static_assert (addressof (j) == &i, "");

struct S { int s; } s;
static_assert (__builtin_addressof (s) == &s, "");
static_assert (addressof (s) == &s, "");

struct T
{
  static T tt;
  constexpr T () : p (addressof (tt)) {}
  constexpr T *operator & () const { return p; }
  T *p;
};
constexpr T t;
T T::tt;
static_assert (&t == __builtin_addressof (T::tt), "");
static_assert (&t == addressof (T::tt), "");

struct S x, y;

constexpr S *
foo (bool b)
{
  return __builtin_addressof (b ? x : y);
}

constexpr S *
bar (bool b, S &c, S &d)
{
  return __builtin_addressof (b ? c : d);
}

static_assert (foo (false) == &y, "");
static_assert (foo (true) == &x, "");
static_assert (bar (false, y, x) == &x, "");
static_assert (bar (true, y, x) == &y, "");

constexpr S *
foo2 (bool b)
{
  return addressof (b ? x : y);
}

constexpr S *
bar2 (bool b, S &c, S &d)
{
  return addressof (b ? c : d);
}

static_assert (foo2 (false) == &y, "");
static_assert (foo2 (true) == &x, "");
static_assert (bar2 (false, y, x) == &x, "");
static_assert (bar2 (true, y, x) == &y, "");

constexpr int a = 1;
static_assert (__builtin_addressof (a) == &a, "");
static_assert (addressof (a) == &a, "");
constexpr int c[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

void
baz ()
{
}

int
main ()
{
  if (__builtin_addressof (T::tt) == __builtin_addressof (t)
      || addressof (T::tt) == addressof (t)
      || &T::tt != &t
      || __builtin_addressof (baz) != baz
      || addressof (baz) != baz)
    __builtin_abort ();

  // reinterpret casts are not constexprs
  if (! (((int *) __builtin_addressof (s) == &s.s)
	 && ((int *) addressof (s) == &s.s)
	 && (__builtin_addressof (t) == (const T *) &t.p)
	 && (addressof (t) == (const T *) &t.p)
	 && ((const int *) __builtin_addressof (c) == &c[0])
	 && ((const int *) addressof (c) == &c[0])))
    __builtin_abort ();

  return 0;
}
