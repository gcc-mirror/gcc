// PR debug/65678
// { dg-do compile }

long long v;

static int
bar (double x)
{
#if __SIZEOF_DOUBLE__ == __SIZEOF_LONG_LONG__
  __builtin_memmove (&v, &x, sizeof v);
#else
  (void) x;
  v = 0;
#endif
  return v;
}

struct A
{
  A (double x) : a (bar (x)) {}
  int m1 ();
  int m2 () { int b = a; return b; }
  int a;
};

void foo ();

void
baz (double x)
{
  int c = A (x).m2 ();
  int d = A (x).m1 ();
  if (d)
    foo ();
}
