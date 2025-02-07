// PR c++/86769
// { dg-do compile { target c++20 } }

struct A {
  int *a;
  constexpr A (int x) : a (new int (x)) {}
  constexpr A (const A &x) : a (new int (x.a[0])) {}
  constexpr ~A () { delete a; }
  constexpr operator bool () { return *a != 0; }
};

constexpr int
foo ()
{
  int i = 0;
  for (A a = 0; A b = a.a[0] < 16; a.a[0] += b.a[0])
    i += a.a[0] + b.a[0];
  return i;
}

static_assert (foo () == 136);

constexpr int
bar ()
{
  int i = 0;
  A a = 0;
  while (A b = a.a[0] < 15)
    {
      i += a.a[0] + b.a[0];
      a.a[0] += b.a[0];
    }
  return i;
}

static_assert (bar () == 120);
