#include "tree-vect.h"

#ifndef N
#define N 64
#endif

char a[N];
float b[N];
long long l[N], m[N];

__attribute__((noinline, noclone)) int
f1 (void)
{
  int i;
  for (i = 0; i < N; i++)
    a[i] = i;
}

__attribute__((noinline, noclone)) int
f2 (void)
{
  int i;
  for (i = 0; i < N; i++)
    b[i] = (double) i;
}

__attribute__((noinline, noclone)) int
f3 (void)
{
  int i;
  for (i = 0; i < N; i++)
    l[i] = (long long) i * (i + 7);
}

__attribute__((noinline, noclone)) int
f4 (void)
{
  int i;
  for (i = 0; i < N; i++)
    m[i] = (long long) i * 7;
}

int
main ()
{
  int i;

  check_vect ();
  f1 ();
  f2 ();
  f3 ();
  f4 ();
  for (i = 0; i < N; i++)
    if (a[i] != i || b[i] != i || l[i] != i * (i + 7LL) || m[i] != i * 7LL)
      abort ();
  return 0;
}

