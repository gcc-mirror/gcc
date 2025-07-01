// PR c++/120471
// { dg-do compile }

extern int a1[], a2[], a3[], a4[];

int corge (int);

int
foo (int p)
{
  return (p ? a1 : a2)[1];
}

int
bar (int p, int q)
{
  return (p ? a1 : a2)[q];
}

int
garply (int p, int q)
{
  return (p ? a1 : a2)[corge (q)];
}

int
baz (int p, int q)
{
  return (p ? q ? a1 : a2 : q ? a3 : a4)[1];
}

int
qux (int p, int q, int r)
{
  return (p ? q ? a1 : a2 : q ? a3 : a4)[r];
}

int
fred (int p, int q, int r)
{
  return (p ? q ? a1 : a2 : q ? a3 : a4)[corge (r)];
}
