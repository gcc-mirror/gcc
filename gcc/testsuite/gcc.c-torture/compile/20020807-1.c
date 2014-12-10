/* { dg-skip-if "requires alloca" { ! alloca } { "-O0" } { "" } } */
int x;

static int
__attribute__ ((noinline))
foo (void)
{
  return 0;
}

static void
__attribute__ ((noinline))
bar (void)
{
}

static inline void
baz (void)
{
  char arr[x];

lab:
  if (foo () == -1)
    {
      bar ();
      goto lab;
    }
}

void
test (void)
{
  baz ();
}
