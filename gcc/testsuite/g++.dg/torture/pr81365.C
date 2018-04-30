// PR tree-optimization/81365
// { dg-do run }

struct A { unsigned a; };

struct B {
  B (const A *x)
  {
    __builtin_memcpy (b, x, 3 * sizeof (A));
    __builtin_memcpy (c, x + 3, sizeof (A));
    __builtin_memset (c + 1, 0, sizeof (A));
  }
  bool
  foo (unsigned x)
  {
    A *it = c;
    if (it->a == x || (++it)->a == x)
      {
	A t(b[0]);
	b[0] = *it;
	*it = t;
	return true;
      }
    return false;
  }
  A b[3];
  A c[2];
};

int
main ()
{
  A x[] = { 4, 8, 12, 18 };
  B y(x);
  if (!y.foo (18))
    __builtin_abort ();
  if (!y.foo (4))
    __builtin_abort ();
}
