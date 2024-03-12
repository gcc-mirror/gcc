/* { dg-require-effective-target vect_cond_mixed } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fno-ipa-icf" } */

#include "tree-vect.h"

#define N 1024
float a[N], b[N], c[N], d[N];
int j[N];
unsigned char k[N];

__attribute__((noinline, noclone)) void
f1 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    {
      unsigned int x = a[i] < b[i] ? -1 : 0;
      unsigned int y = c[i] < d[i] ? -1 : 0;
      j[i] = (x & y) >> 31;
    }
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    {
      int x = a[i] < b[i];
      int y = c[i] < d[i];
      j[i] = x & y;
    }
}

__attribute__((noinline, noclone)) void
f3 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    j[i] = (a[i] < b[i]) & (c[i] < d[i]);
}

__attribute__((noinline, noclone)) void
f4 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    {
      int x = a[i] < b[i];
      int y = c[i] < d[i];
      k[i] = x & y;
    }
}

__attribute__((noinline, noclone)) void
f5 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    k[i] = (a[i] < b[i]) & (c[i] < d[i]);
}

__attribute__((noinline, noclone)) void
f6 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    {
      unsigned int x = a[i] < b[i] ? -1 : 0;
      unsigned int y = c[i] < d[i] ? -1 : 0;
      j[i] = (x | y) >> 31;
    }
}

__attribute__((noinline, noclone)) void
f7 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    {
      int x = a[i] < b[i];
      int y = c[i] < d[i];
      j[i] = x | y;
    }
}

__attribute__((noinline, noclone)) void
f8 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    j[i] = (a[i] < b[i]) | (c[i] < d[i]);
}

__attribute__((noinline, noclone)) void
f9 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    {
      int x = a[i] < b[i];
      int y = c[i] < d[i];
      k[i] = x | y;
    }
}

__attribute__((noinline, noclone)) void
f10 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    k[i] = (a[i] < b[i]) | (c[i] < d[i]);
}

int
main ()
{
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      switch (i % 9)
	{
	case 0: asm (""); a[i] = - i - 1; b[i] = i + 1; break;
	case 1: a[i] = 0; b[i] = 0; break;
	case 2: a[i] = i + 1; b[i] = - i - 1; break;
	case 3: a[i] = i; b[i] = i + 7; break;
	case 4: a[i] = i; b[i] = i; break;
	case 5: a[i] = i + 16; b[i] = i + 3; break;
	case 6: a[i] = - i - 5; b[i] = - i; break;
	case 7: a[i] = - i; b[i] = - i; break;
	case 8: a[i] = - i; b[i] = - i - 7; break;
	}
    }
  for (i = 0; i < N; i++)
    {
      switch ((i / 9) % 3)
	{
	case 0: c[i] = a[i / 9]; d[i] = b[i / 9]; break;
	case 1: c[i] = a[i / 9 + 3]; d[i] = b[i / 9 + 3]; break;
	case 2: c[i] = a[i / 9 + 6]; d[i] = b[i / 9 + 6]; break;
	}
    }
  f1 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (j[i] != ((i % 3) == 0 && ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (j, -6, sizeof (j));
  f2 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (j[i] != ((i % 3) == 0 && ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (j, -6, sizeof (j));
  f3 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (j[i] != ((i % 3) == 0 && ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (j, -6, sizeof (j));
  f4 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 && ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, -6, sizeof (k));
  f5 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 && ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, -6, sizeof (k));
  f6 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (j[i] != ((i % 3) == 0 || ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (j, -6, sizeof (j));
  f7 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (j[i] != ((i % 3) == 0 || ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (j, -6, sizeof (j));
  f8 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (j[i] != ((i % 3) == 0 || ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (j, -6, sizeof (j));
  f9 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 || ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, -6, sizeof (k));
  f10 ();
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 || ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, -6, sizeof (k));

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 10 "vect" } } */
