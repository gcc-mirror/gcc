extern void anything(int);

static int foo(int i);

static int bar(int i) { foo(i); }

extern int j;

static int foo(int i)
{
  if (j)
    anything(j);
  return bar(i);
}

int baz()
{
  foo(0);
  if (baz())
    return 1;
  return 0;
}
