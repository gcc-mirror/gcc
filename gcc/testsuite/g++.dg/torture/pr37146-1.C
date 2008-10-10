// PR c++/37146
// { dg-do run }

extern "C" void abort ();
int a, b;
struct A { int i:8; int j:8; int k:16; int l:32; } c;

void
f1 (int x, int y)
{
  (x ? a : b) = y;
}

void
f2 (int x, int y)
{
  (x ? c.i : c.j) = y;
}

void
f3 (int x, int y)
{
  (x ? c.i : a) = y;
}

void
f4 (int x, int y)
{
  (x ? c.i : c.k) = y;
}

void
f5 (int x, int y)
{
  (x ? c.l : b) = y;
}

#define CHECK(var, exp)		\
  do				\
    {				\
      if (var != exp)		\
	 abort ();		\
      var = -1;			\
      if (a != -1		\
	  || b != -1		\
	  || c.i != -1		\
	  || c.j != -1		\
	  || c.k != -1		\
	  || c.l != -1)		\
	abort ();		\
    }				\
  while (0)

int
main ()
{
  a = -1;
  b = -1;
  c.i = -1;
  c.j = -1;
  c.k = -1;
  c.l = -1;
  f1 (1, 264);
  CHECK (a, 264);
  f1 (0, 264);
  CHECK (b, 264);
  f2 (1, 112);
  CHECK (c.i, 112);
  f2 (0, 112);
  CHECK (c.j, 112);
  f3 (1, 26);
  CHECK (c.i, 26);
  f3 (0, 26);
  CHECK (a, 26);
  f4 (1, 107);
  CHECK (c.i, 107);
  f4 (0, 107);
  CHECK (c.k, 107);
  f5 (1, 95);
  CHECK (c.l, 95);
  f5 (0, 95);
  CHECK (b, 95);
}
