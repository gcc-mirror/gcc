// PR c++/89336
// { dg-do compile { target c++14 } }

constexpr int
foo ()
{
  int a[16] = {};
  int r = 0;
  a[15] = a[14] = a[13] = a[12] = a[11] = a[10] = a[9] = a[8]
    = a[7] = a[6] = a[5] = a[4] = a[3] = a[2] = a[1] = a[0] = 5;
  for (int i = 0; i < 16; ++i)
    r += a[i];
  return r;
}

static_assert (foo () == 16 * 5, "");

struct A { int a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p; };

constexpr int
bar ()
{
  A a {};
  a.p = a.o = a.n = a.m = a.l = a.k = a.j = a.i
    = a.h = a.g = a.f = a.e = a.d = a.c = a.b = a.a = 8;
  return a.a + a.b + a.c + a.d + a.e + a.f + a.g + a.h
	 + a.i + a.j + a.k + a.l + a.m + a.n + a.o + a.p;
}

static_assert (bar () == 16 * 8, "");

constexpr int
baz ()
{
  int a[16] = {};
  int r = 0;
  a[0] = a[1] = a[2] = a[3] = a[4] = a[5] = a[6] = a[7]
    = a[8] = a[9] = a[10] = a[11] = a[12] = a[13] = a[14] = a[15] = 7;
  for (int i = 0; i < 16; ++i)
    r += a[i];
  return r;
}

static_assert (baz () == 16 * 7, "");

constexpr int
qux ()
{
  A a {};
  a.a = a.b = a.c = a.d = a.e = a.f = a.g = a.h
    = a.i = a.j = a.k = a.l = a.m = a.n = a.o = a.p = 6;
  return a.a + a.b + a.c + a.d + a.e + a.f + a.g + a.h
	 + a.i + a.j + a.k + a.l + a.m + a.n + a.o + a.p;
}

static_assert (qux () == 16 * 6, "");
