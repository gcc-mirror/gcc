// PR c++/102490
// { dg-do run { target c++20 } }

struct A
{
  unsigned char a : 1;
  unsigned char b : 1;
  constexpr bool operator== (const A &) const = default;
};

struct B
{
  unsigned char a : 8;
  int : 0;
  unsigned char b : 7;
  constexpr bool operator== (const B &) const = default;
};

struct C
{
  unsigned char a : 3;
  unsigned char b : 1;
  constexpr bool operator== (const C &) const = default;
};

void
foo (C &x, int y)
{
  x.b = y;
}

int
main ()
{
  A a{}, b{};
  B c{}, d{};
  C e{}, f{};
  a.b = 1;
  d.b = 1;
  foo (e, 0);
  foo (f, 1);
  return a == b || c == d || e == f;
}
