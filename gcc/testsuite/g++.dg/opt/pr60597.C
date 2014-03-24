// PR middle-end/60597
// { dg-do compile }
// { dg-options "-O2 -g" }

struct A
{
  int foo () const;
  int bar () const;
  int a;
};

struct B
{
  int foo ();
  int bar ();
};

int *c, d;

int
A::foo () const
{
  int b = a >> 16;
  return b;
}

int
A::bar () const
{
  int b = a;
  return b;
}

void
baz (A &x, B h, int i, int j)
{
  for (; i < h.bar (); ++i)
    for (; h.foo (); ++j)
      {
	int g = x.foo ();
	int f = x.bar ();
	int e = c[0] & 1;
	d = (e << 1) | (g << 16) | (f & 1);
	c[j] = 0;
      }
}
