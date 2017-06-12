// PR c++/79681
// { dg-do compile { target c++14 } }
// { dg-options "-O2" }

struct A
{
  char i : 4;
  char k : 1;
  char l : 3;
};
struct B
{
  char j : 4;
};
struct C
{
  long long u;
  A a[1];
  B b[1];
};

constexpr bool
foo ()
{
  C c = { 0, { { 5, 0, 2 } }, { { 6 } } };
  C d = { 0, { { 6, 0, 1 } }, { { 5 } } };
  return c.a[0].i == d.a[0].i && c.b[0].j == d.b[0].j;
}

constexpr bool
bar ()
{
  C c = { 0, { { 5, 0, 2 } }, { { 6 } } };
  C d = { 0, { { 6, 0, 1 } }, { { 5 } } };
  return c.a[0].i == d.a[0].i && c.a[0].l == d.a[0].l;
}

static_assert (foo () == false, "");
static_assert (bar () == false, "");
