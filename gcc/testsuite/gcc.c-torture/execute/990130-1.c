int count = 0;
int dummy;

static int *
bar(void)
{
  ++count;
  return &dummy;
}

static void
foo(void)
{
  asm("" : "+r"(*bar()));
}

main()
{
  foo();
  if (count != 1)
    abort();
  exit(0);
}
