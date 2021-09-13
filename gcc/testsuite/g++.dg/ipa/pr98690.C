/* { dg-do run } */
/* { dg-options "-O2 -fnon-call-exceptions" } */

int g;
volatile int v;

static int * __attribute__((noinline))
almost_useless_return (void)
{
  v = 1;
  return &g;
}

static void __attribute__((noinline))
foo (void)
{
  int *p = almost_useless_return ();
  int i = *p;
  v = 2;
}

int
main (int argc, char *argv[])
{
  foo ();
  return 0;
}
