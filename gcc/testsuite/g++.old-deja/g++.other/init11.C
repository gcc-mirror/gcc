// { dg-do run  }
// Check that elements for which no explicit initializer was given are
// default-initialized properly.

extern "C" int printf (const char *, ...);

struct A
{
  int i;
  A(): i (42) { }
  A(int j): i(j) { }
};

A ar[4] = { 1, 2 };

struct B
{
  A a1, a2, a3, a4;
};

B b = { 1, 2 };

struct C
{
  A ar[4];
};

C c = { 1, 2 };

int
main ()
{
  printf ("%d %d %d %d\n%d %d %d %d\n%d %d %d %d\n",
	  ar[0].i, ar[1].i, ar[2].i, ar[3].i,
	  b.a1.i, b.a2.i, b.a3.i, b.a4.i,
	  c.ar[1-1].i, c.ar[2-1].i, c.ar[3-1].i, c.ar[4-1].i);

  return (b.a4.i != 42 || c.ar[3].i != 42);
}
