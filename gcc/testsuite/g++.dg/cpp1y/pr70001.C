// PR c++/70001
// { dg-do compile { target c++14 } }

struct B
{
  int a;
  constexpr B () : a (0) { }
  constexpr B (int x) : a (x) { }
};
struct C
{
  B c;
  constexpr C () : c (0) { }
};
struct A
{
  B b[1 << 4];
};
struct D
{
  C d[1 << 4];
};

constexpr int
foo (int a, int b)
{
  A c;
  c.b[a].a += b;
  c.b[b].a += a;
  return c.b[0].a + c.b[a].a + c.b[b].a;
}

constexpr int
bar (int a, int b)
{
  D c;
  c.d[a].c.a += b;
  c.d[b].c.a += a;
  return c.d[0].c.a + c.d[a].c.a + c.d[b].c.a;
}

constexpr int d = foo (1, 2);
constexpr int e = foo (0, 3);
constexpr int f = foo (2, 4);
constexpr int g = bar (1, 2);
constexpr int h = bar (0, 3);
constexpr int i = bar (2, 4);
static_assert (d == 3 && e == 6 && f == 6, "");
static_assert (g == 3 && h == 6 && i == 6, "");
