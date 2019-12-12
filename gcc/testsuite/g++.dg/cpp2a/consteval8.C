// { dg-do compile }
// { dg-options "-std=c++2a" }

struct S { consteval S () : a (1), b (2) { a++; b++; } consteval S (int x) : a (x), b (x) { a++; b--; } int a, b; };
S c;
S d = 25;

int
foo ()
{
  S a;
  a.b++;
  c = a;
  S b = 12;
  c.a += b.a;
  c.b += b.b;
  S e[2];
  S f[2] = { 1, 2 };
  return S ().a + e[1].a + f[0].b;
}

constexpr S g;
constexpr S h = 42;
constexpr S i[2];
constexpr S j[2] = { 3, 4 };
static_assert (g.a == 2 && g.b == 3);
static_assert (h.a == 43 && h.b == 41);
static_assert (i[0].a == 2 && i[0].b == 3 && i[1].a == 2 && i[1].b == 3);
static_assert (j[0].a == 4 && j[0].b == 2 && j[1].a == 5 && j[1].b == 3);
