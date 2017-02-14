// PR c++/77553
// { dg-do compile { target c++14 } }

constexpr void
bar (int *x)
{
  int i = 0;
  x[i++] = 1;
  x[3] = i;
}

constexpr int
foo ()
{
  int a[] = { 0, 0, 0, 0 };
  bar (a);

  return a[0] + 8 * a[1] + 64 * a[2] + 512 * a[3];
}

constexpr int b = foo ();

int
main ()
{
  static_assert (b == 513, "");
  if (foo () != 513)
    __builtin_abort ();
}
