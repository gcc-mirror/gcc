extern void abort ();

void __attribute__((noinline))
bar (unsigned long long i)
{
  if (i)
    abort ();
}

static void __attribute__((always_inline))
foo (unsigned long long *r)
{
  int i;

  for (i = 0; ; i++)
    if (*r & ((unsigned long long)1 << (63 - i)))
      break;

  bar (i);
}

void __attribute__((noinline))
do_test (unsigned long long *r)
{
  int i;

  for (i = 0; i < 2; ++i)
    foo (r);
}

int main()
{
  unsigned long long r = 0x8000000000000001ull;

  do_test (&r);
  return 0;
}
