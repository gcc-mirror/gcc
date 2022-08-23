/* { dg-do run } */
/* { dg-options "-O2 -Wno-pointer-to-int-cast" } */

struct b {int b;};
struct a {int a; struct b b;};

long i;

__attribute__ ((noinline))
static void test2 (struct b *b)
{
  if (((int)b)&4)
    __builtin_abort ();
}

__attribute__ ((noinline))
static void
test (struct a *a)
{
  test2(a? &a->b : 0);
}

int
main()
{
  test(0);
  return 0;
}
