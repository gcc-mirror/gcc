/* { dg-do run } */
/* { dg-options "-O2" } */

typedef unsigned long gr;

template <int l, int r>
struct mask {
  enum { value = (1ul << r) - (1ul << l) };
};

template <int l>
struct mask<l, sizeof (gr) * __CHAR_BIT__> {
  enum { value = -(1ul << l) };
};

__attribute__ ((noipa)) void
test (gr a, gr b, gr mask, gr out)
{
  if (((a & mask) | (b & ~mask)) != out)
    __builtin_abort ();
}

__attribute__ ((noipa)) gr
no_optimize (gr x)
{
  return x;
}

template <int l, int r>
struct test1 {
  static void
  run (void)
  {
    gr m = mask<l, r>::value;
    gr a = no_optimize (-1ul);
    gr b = no_optimize (0);

    test (a, b, m, (a & m) | (b & ~m));
    test (a, b, ~m, (a & ~m) | (b & m));
    test (a, 0, ~m, a & ~m);

    test1<l, r + 1>::run ();
  }
};

template <int l>
struct test1<l, sizeof (gr) * __CHAR_BIT__ + 1> {
  static void run (void) {}
};

template <int l>
void
test2 (void)
{
  test1<l, l + 1>::run ();
  test2<l + 1> ();
}

template <> void test2<sizeof (gr) * __CHAR_BIT__> (void) {}

int
main ()
{
  test2<0> ();
}
