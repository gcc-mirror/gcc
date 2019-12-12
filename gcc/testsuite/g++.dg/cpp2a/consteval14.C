// { dg-do compile }
// { dg-options "-std=c++2a" }

struct S { consteval S () : a (1), b (2) { a++; b++; } consteval S (int x) : a (x), b (x) { a++; b--; } int a, b; };
S c;

template <int N>
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
  const S g;
  c.a += g.a;
  c.b += g.b;
  const S h = 12;
  c.a += h.a;
  c.b += h.b;
  const S i[2];
  const S j[2] = { 1, 2 };
  return S ().a + e[1].a + f[0].b + i[0].a + j[1].b;
}

int x = foo <2> ();
