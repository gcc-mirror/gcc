/* PR target/11087
   This testcase was miscompiled on ppc64, because basic_induction_var called
   convert_modes, yet did not expect it to emit any new instructions.
   Those were emitted at the end of the function and destroyed during life
   analysis, while the program used uninitialized pseudos created by
   convert_modes.  */

struct A
{
  unsigned short a1;
  unsigned long a2;
};

struct B
{
  int b1, b2, b3, b4, b5;
};

struct C
{
  struct B c1[1];
  int c2, c3;
};

static
int foo (int x)
{
  return  x < 0 ? -x : x;
}

int bar (struct C *x, struct A *y)
{
  int a = x->c3;
  const int b = y->a1 >> 9;
  const unsigned long c = y->a2;
  int d = a;
  unsigned long e, f;

  f = foo (c - x->c1[d].b4);
  do
    {
      if (d <= 0)
	d = x->c2;
      d--;

      e = foo (c-x->c1[d].b4);
      if (e < f)
	a = d;
    }
  while (d != x->c3);
  x->c1[a].b4 = c + b;
  return a;
}

int
main ()
{
  struct A a;
  struct C b;
  int c;

  a.a1 = 512;
  a.a2 = 4242;
  __builtin_memset (&b, 0, sizeof (b));
  b.c1[0].b3 = 424242;
  b.c2 = 1;
  c = bar (&b, &a);
  return 0;
}
