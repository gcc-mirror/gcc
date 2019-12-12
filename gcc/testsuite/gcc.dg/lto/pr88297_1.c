extern void __attribute__ ((noipa))
use (int v);


static int __attribute__ ((noinline))
foo (int arg)
{
  return 8 * arg;
}

static int __attribute__ ((noinline))
bar (int arg)
{
  return arg * arg + 3;
}

int __attribute__ ((noinline))
entry2 (void)
{
  use (bar (3));
  use (bar (4));
  use (foo (5));
  use (foo (6));
  return 0;
}
