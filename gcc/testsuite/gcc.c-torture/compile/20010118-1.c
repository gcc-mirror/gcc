static unsigned int bar(void *h, unsigned int n)
{
  static int i;
  return i++;
}

static void baz(unsigned int *x)
{
  (*x)++;
}

long
foo(void *h, unsigned int l)
{
  unsigned int n;
  long m;
  n = bar(h, 0);
  n = bar(h, n);
  m = ({ baz(&n); 21; });
  return m;
}
