int a[1] = {1};

static int __attribute__((noinline))
foo(int n)
{
  int i, c = 0;
  for (i = 0; i < n; i++)
    c += a[i];
  return c;
}

int b[2] = {2, 3};

static int __attribute__((noinline)) bar(int n)
{
  int i, c = 0;
  for (i = 0; i < n; i++)
    c += b[i];
  return c;
}

extern void abort ();

int main()
{
  if (foo(0) != 0
      || foo(1) != 1
      || bar(0) != 0
      || bar(1) != 2
      || bar(2) != 5)
    abort ();

  return 0;
}

