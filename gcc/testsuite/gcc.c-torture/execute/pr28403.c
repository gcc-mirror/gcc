void abort (void);
void exit (int);

typedef unsigned long long ull;
int global;

int __attribute__((noinline))
foo (int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8)
{
  global = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8;
}

ull __attribute__((noinline))
bar (ull x)
{
  foo (1, 2, 1, 3, 1, 4, 1, 5);
  return x >> global;
}

int
main (void)
{
  if (bar (0x123456789abcdefULL) != (0x123456789abcdefULL >> 18))
    abort ();
  exit (0);
}
