void abort (void);
void exit (int);

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

int
main(void)
{
  foo();
  if (count != 1)
    abort();
  exit(0);
}
