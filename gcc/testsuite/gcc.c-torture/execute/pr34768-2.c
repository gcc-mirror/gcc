int x;

int __attribute__((noinline)) foo (void)
{
  x = -x;
  return 0;
}
int __attribute__((const,noinline)) bar (void)
{
  return 0;
}

int __attribute__((noinline))
test (int c)
{
  int tmp = x;
  int res = (c ? foo : bar) ();
  return tmp + x + res;
}

extern void abort (void);
int main()
{
  x = 1;
  if (test (1) != 0)
    abort ();
  return 0;
}
