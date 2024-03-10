/* { dg-do run } */
/* { dg-options "-O2 -fno-early-inlining -fno-ipa-sra"  } */

struct a {int a;};
static int
foo (struct a *a)
{
  if (!__builtin_constant_p (a->a))
    __builtin_abort ();
  return a->a;
}

static int __attribute__ ((noinline))
bar (struct a *a)
{
  return foo(a);
}

volatile int r;

int main()
{
  struct a a={1};
  r = bar (&a);
  return 0;
}
