/* { dg-require-effective-target vect_cond_mixed } */

#include "tree-vect.h"

#define N 1024
float a[N], b[N], c[N], d[N];
_Bool k[N];

__attribute__((noinline, noclone)) void
f1 (void)
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
f2 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    k[i] = (a[i] < b[i]) & (c[i] < d[i]);
}

__attribute__((noinline, noclone)) void
f3 (void)
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
f4 (void)
{
  int i;
  for (i = 0; i < N; ++i)
    k[i] = (a[i] < b[i]) | (c[i] < d[i]);
}

__attribute__((noinline, noclone)) void
f5 (_Bool *p)
{
  int i;
  for (i = 0; i < N; ++i)
    {
      int x = a[i] < b[i];
      int y = c[i] < d[i];
      p[i] = x & y;
    }
}

__attribute__((noinline, noclone)) void
f6 (_Bool *p)
{
  int i;
  for (i = 0; i < N; ++i)
    p[i] = (a[i] < b[i]) & (c[i] < d[i]);
}

__attribute__((noinline, noclone)) void
f7 (_Bool *p)
{
  int i;
  for (i = 0; i < N; ++i)
    {
      int x = a[i] < b[i];
      int y = c[i] < d[i];
      p[i] = x | y;
    }
}

__attribute__((noinline, noclone)) void
f8 (_Bool *p)
{
  int i;
  for (i = 0; i < N; ++i)
    p[i] = (a[i] < b[i]) | (c[i] < d[i]);
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
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 && ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, 0, sizeof (k));
  f2 ();
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 && ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, 0, sizeof (k));
  f3 ();
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 || ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, 0, sizeof (k));
  f4 ();
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 || ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, 0, sizeof (k));
  f5 (k);
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 && ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, 0, sizeof (k));
  f6 (k);
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 && ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, 0, sizeof (k));
  f7 (k);
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 || ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, 0, sizeof (k));
  f8 (k);
  for (i = 0; i < N; i++)
    if (k[i] != ((i % 3) == 0 || ((i / 9) % 3) == 0))
      abort ();
  __builtin_memset (k, 0, sizeof (k));

  return 0;
}

/* { dg-final { scan-tree-dump-times "note: vectorized 1 loops" 8 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
