// PR debug/80436
// { dg-do compile { target c++11 } }
// { dg-options "-O3 -fcompare-debug" }

void fn (...);
void foo (int, int, int);
struct { int elt1; int bits; } *a;
int b, d;

int
bar (unsigned *x)
{
  if (0)
  next_bit:
    return 1;
  while (1)
    {
      if (b)
	if (a->bits)
	  goto next_bit;
      *x = b;
      if (a->elt1)
	return 0;
      a = 0;
    }
}

enum { C0, C1 } *c;

void
baz ()
{
  int e, m = d;
  for (; e < m; e++)
    {
      if (e < 0)
	foo (0, 0, c[e]);
      unsigned f;
      for (; bar (&f);)
	fn (f);
    }
}
