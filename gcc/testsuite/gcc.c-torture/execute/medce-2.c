
extern void abort ();

static int ok = 0;

int bar(void)
{
  ok |= 1;
  return 1;
}

void bat(void)
{
  ok |= 2;
}

void baz(void)
{
  ok |= 4;
}

void foo()
{
  goto lab;

  if (0)
  {
    if (({lab: bar();}))
      bat ();
    else
      baz ();
  }
}

int main()
{
  foo();
  if (ok != 3)
    abort ();
  return 0;
}

