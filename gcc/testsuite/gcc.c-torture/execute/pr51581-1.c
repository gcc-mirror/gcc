/* PR tree-optimization/51581 */

extern void abort (void);

#define N 4096
int a[N], c[N];
unsigned int b[N], d[N];

__attribute__((noinline, noclone)) void
f1 (void)
{
  int i;
  for (i = 0; i < N; i++)
    c[i] = a[i] / 3;
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  int i;
  for (i = 0; i < N; i++)
    d[i] = b[i] / 3;
}

__attribute__((noinline, noclone)) void
f3 (void)
{
  int i;
  for (i = 0; i < N; i++)
    c[i] = a[i] / 18;
}

__attribute__((noinline, noclone)) void
f4 (void)
{
  int i;
  for (i = 0; i < N; i++)
    d[i] = b[i] / 18;
}

__attribute__((noinline, noclone)) void
f5 (void)
{
  int i;
  for (i = 0; i < N; i++)
    c[i] = a[i] / 19;
}

__attribute__((noinline, noclone)) void
f6 (void)
{
  int i;
  for (i = 0; i < N; i++)
    d[i] = b[i] / 19;
}

#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8
__attribute__((noinline, noclone)) void
f7 (void)
{
  int i;
  for (i = 0; i < N; i++)
    c[i] = (int) ((unsigned long long) (a[i] * 0x55555556LL) >> 32) - (a[i] >> 31);
}

__attribute__((noinline, noclone)) void
f8 (void)
{
  int i;
  for (i = 0; i < N; i++)
    d[i] = ((unsigned int) ((b[i] * 0xaaaaaaabULL) >> 32) >> 1);
}

__attribute__((noinline, noclone)) void
f9 (void)
{
  int i;
  for (i = 0; i < N; i++)
    c[i] = (((int) ((unsigned long long) (a[i] * 0x38e38e39LL) >> 32)) >> 2) - (a[i] >> 31);
}

__attribute__((noinline, noclone)) void
f10 (void)
{
  int i;
  for (i = 0; i < N; i++)
    d[i] = (unsigned int) ((b[i] * 0x38e38e39ULL) >> 32) >> 2;
}

__attribute__((noinline, noclone)) void
f11 (void)
{
  int i;
  for (i = 0; i < N; i++)
    c[i] = (((int) ((unsigned long long) (a[i] * 0x6bca1af3LL) >> 32)) >> 3) - (a[i] >> 31);
}

__attribute__((noinline, noclone)) void
f12 (void)
{
  int i;
  for (i = 0; i < N; i++)
    {
      unsigned int tmp = (b[i] * 0xaf286bcbULL) >> 32;
      d[i] = (((b[i] - tmp) >> 1) + tmp) >> 4;
    }
}
#endif

int
main ()
{
  int i;
  for (i = 0; i < N; i++)
    {
      asm ("");
      a[i] = i - N / 2;
      b[i] = i;
    }
  a[0] = -__INT_MAX__ - 1;
  a[1] = -__INT_MAX__;
  a[N - 1] = __INT_MAX__;
  b[N - 1] = ~0;
  f1 ();
  f2 ();
  for (i = 0; i < N; i++)
    if (c[i] != a[i] / 3 || d[i] != b[i] / 3)
      abort ();
  f3 ();
  f4 ();
  for (i = 0; i < N; i++)
    if (c[i] != a[i] / 18 || d[i] != b[i] / 18)
      abort ();
  f5 ();
  f6 ();
  for (i = 0; i < N; i++)
    if (c[i] != a[i] / 19 || d[i] != b[i] / 19)
      abort ();
#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8
  f7 ();
  f8 ();
  for (i = 0; i < N; i++)
    if (c[i] != a[i] / 3 || d[i] != b[i] / 3)
      abort ();
  f9 ();
  f10 ();
  for (i = 0; i < N; i++)
    if (c[i] != a[i] / 18 || d[i] != b[i] / 18)
      abort ();
  f11 ();
  f12 ();
  for (i = 0; i < N; i++)
    if (c[i] != a[i] / 19 || d[i] != b[i] / 19)
      abort ();
#endif
  return 0;
}
