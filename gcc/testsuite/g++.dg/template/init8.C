// PR c++/36089
// { dg-do run }

extern "C" void abort ();

int f ()
{
  const int c(2);
  int d[c] = { 0, 0 };
  return d[0] + sizeof d;
}

struct A
{
  static int f ()
  {
    const int c(2);
    int d[c] = { 0, 0 };
    return d[0] + sizeof d;
  }
};

template <int> struct B
{
  static int f ()
  {
    const int c = 2;
    int d[c] = { 0, 0 };
    return d[0] + sizeof d;
  }
};

template <int> struct C
{
  static int f ()
  {
    const int c(2);
    int d[c] = { 0, 0 };
    return d[0] + sizeof d;
  }
};

template <int> struct D
{
  static int f ()
  {
    const int e(2);
    const int c(e);
    int d[c] = { 0, 0 };
    return d[0] + sizeof d;
  }
};

int
main (void)
{
  int v = f ();
  if (v != 2 * sizeof (int))
    abort ();
  if (v != A::f ())
    abort ();
  if (v != B<6>::f ())
    abort ();
  if (v != C<0>::f ())
    abort ();
  if (v != D<1>::f ())
    abort ();
}
