// PR middle-end/60597
// { dg-do compile }
// { dg-options "-O2 -g" }

typedef int int32_t __attribute__((mode (__SI__)));

struct A
{
  int32_t foo () const;
  int32_t bar () const;
  int32_t a;
};

struct B
{
  int32_t foo ();
  int32_t bar ();
};

int32_t *c, d;

int32_t
A::foo () const
{
  int32_t b = a >> 16;
  return b;
}

int32_t
A::bar () const
{
  int32_t b = a;
  return b;
}

void
baz (A &x, B h, int32_t i, int32_t j)
{
  for (; i < h.bar (); ++i)
    for (; h.foo (); ++j)
      {
	int32_t g = x.foo ();
	int32_t f = x.bar ();
	int32_t e = c[0] & 1;
	d = (e << 1) | (g << 16) | (f & 1);
	c[j] = 0;
      }
}
