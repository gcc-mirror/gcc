// PR c++/86769
// { dg-do run { target c++11 } }

int d, e, f, g, h;
struct A {
  int a;
  A (int x) : a(x) { ++d; ++h; }
  ~A () { --d; ++h; }
  A (const A &x) : a(x.a) { ++d; ++h; }
  operator bool () { return a != 0; }
};
struct B {
  B () { ++e; ++h; }
  ~B () { --e; ++h; }
  B (const B &) { ++e; ++h; }
};

int
foo (B, int x)
{
  if (e != 1)
    __builtin_abort ();
  if (f ? x - 1 != (f - 1) % 3 : x)
    __builtin_abort ();
  ++f;
  if (x == 1)
    return ++g < 3;
  return 0;
}

int
bar (int n)
{
  if (e != 0 || d != n)
    __builtin_abort ();
  return 0;
}

int
main ()
{
  for (A a = (bar (0), foo (B {}, 0));
       A b = (bar (1), foo (B {}, 1));
       bar (2), foo (B {}, 3))
    A c = (bar (2), foo (B {}, 2));
  if (f != 8 || h != 28 || d || e)
    __builtin_abort ();
  f = 0; g = -2; h = 0;
  for (A a = (bar (0), foo (B {}, 0));
       A b = (bar (1), foo (B {}, 1));
       bar (2), foo (B {}, 3))
    {
      A c = (bar (2), foo (B {}, 2));
      bar (3);
    }
  if (f != 14 || h != 48 || d || e)
    __builtin_abort ();
  f = 0; g = 0; h = 0;
  {
    A a = (bar (0), foo (B {}, 0));
    while (A b = (bar (1), foo (B {}, 1)))
      {
	A c = (bar (2), foo (B {}, 2));
	bar (3);
	foo (B {}, 3);
      }
  }
  if (f != 8 || h != 28 || d || e)
    __builtin_abort ();
  f = 0; g = -5; h = 0;
  for (A a = (bar (0), foo (B {}, 0));
       A b = (bar (1), foo (B {}, 1));
       bar (2), foo (B {}, 3))
    {
      if (f == 5)
	{
	  bar (2);
	  foo (B {}, 2);
	  bar (2);
	  continue;
	}
      if (f == 11)
	break;
      A c = (bar (2), foo (B {}, 2));
      bar (3);
    }
  if (f != 11 || h != 36 || d || e)
    __builtin_abort ();
  f = 0; g = -5; h = 0;
  {
    A a = (bar (0), foo (B {}, 0));
    while (A b = (bar (1), foo (B {}, 1)))
      {
	if (f == 5)
	  {
	    bar (2);
	    foo (B {}, 2);
	    bar (2);
	    foo (B {}, 3);
	    bar (2);
	    continue;
	  }
	else if (f == 11)
	  break;
	A c = (bar (2), foo (B {}, 2));
	bar (3);
	foo (B {}, 3);
      }
  }
  if (f != 11 || h != 36 || d || e)
    __builtin_abort ();
}
