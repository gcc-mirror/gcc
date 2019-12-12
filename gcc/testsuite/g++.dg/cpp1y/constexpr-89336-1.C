// PR c++/89336
// { dg-do compile { target c++14 } }

template <typename T, int N> struct A {
  T a[N];
  constexpr T &operator[] (int x) { return a[x]; }
  constexpr const T &operator[] (int x) const { return a[x]; }
};

constexpr A<int, 16>
foo ()
{
  A<int, 16> r{};
  for (int i = 0; i < 6; ++i)
    r[i + 8] = r[i] = i + 1;
  return r;
}

constexpr auto x = foo ();
static_assert (x[0] == 1, "");
static_assert (x[1] == 2, "");
static_assert (x[2] == 3, "");
static_assert (x[3] == 4, "");
static_assert (x[4] == 5, "");
static_assert (x[5] == 6, "");
static_assert (x[6] == 0, "");
static_assert (x[7] == 0, "");
static_assert (x[8] == 1, "");
static_assert (x[9] == 2, "");
static_assert (x[10] == 3, "");
static_assert (x[11] == 4, "");
static_assert (x[12] == 5, "");
static_assert (x[13] == 6, "");
static_assert (x[14] == 0, "");
static_assert (x[15] == 0, "");
