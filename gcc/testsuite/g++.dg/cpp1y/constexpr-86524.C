// PR c++/86524
// { dg-do run { target c++14 } }
// { dg-options "-O2" }

extern "C" void abort ();
typedef __UINTPTR_TYPE__ uintptr_t;

constexpr bool
foo (const int *x, const int *y)
{
  if (__builtin_constant_p (x < y))
    return x < y;
  return (uintptr_t) x < (uintptr_t) y;
}

void
bar ()
{
  constexpr int x = 0;
  static_assert (!(&x < &x));
  static_assert (!foo (&x, &x));
}

constexpr void
baz ()
{
  constexpr int x = 0;
  static_assert (!(&x < &x));
  static_assert (!foo (&x, &x));
}

int i, j;

int
main ()
{
  bar ();
  baz ();
  if (!(foo (&i, &j) ^ foo (&j, &i)))
    abort ();
}
