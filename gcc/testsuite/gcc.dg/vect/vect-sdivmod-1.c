#include "tree-vect.h"

extern void abort (void);
int a[4096];

__attribute__((noinline, noclone)) void
f1 (int x)
{
  int i, j;
  for (i = 1; i <= x; i++)
    {
      j = a[i] >> 8;
      j = 1 + (j / 2);
      a[i] = j << 8;
    }
}

__attribute__((noinline, noclone)) void
f2 (int x)
{
  int i, j;
  for (i = 1; i <= x; i++)
    {
      j = a[i] >> 8;
      j = 1 + (j / 16);
      a[i] = j << 8;
    }
}

__attribute__((noinline, noclone)) void
f3 (int x)
{
  int i, j;
  for (i = 1; i <= x; i++)
    {
      j = a[i] >> 8;
      j = 1 + (j % 2);
      a[i] = j << 8;
    }
}

__attribute__((noinline, noclone)) void
f4 (int x)
{
  int i, j;
  for (i = 1; i <= x; i++)
    {
      j = a[i] >> 8;
      j = 1 + (j % 16);
      a[i] = j << 8;
    }
}

int
main ()
{
  int i;
  check_vect ();
  for (i = 0; i < 4096; i++)
    {
      asm ("");
      a[i] = (i - 2048) << 8;
    }
  f1 (4095);
  if (a[0] != (-2048 << 8))
    abort ();
  for (i = 1; i < 4096; i++)
    if (a[i] != ((1 + ((i - 2048) / 2)) << 8))
      abort ();
    else
      a[i] = (i - 2048) << 8;
  f2 (4095);
  if (a[0] != (-2048 << 8))
    abort ();
  for (i = 1; i < 4096; i++)
    if (a[i] != ((1 + ((i - 2048) / 16)) << 8))
      abort ();
    else
      a[i] = (i - 2048) << 8;
  f3 (4095);
  if (a[0] != (-2048 << 8))
    abort ();
  for (i = 1; i < 4096; i++)
    if (a[i] != ((1 + ((i - 2048) % 2)) << 8))
      abort ();
    else
      a[i] = (i - 2048) << 8;
  f4 (4095);
  if (a[0] != (-2048 << 8))
    abort ();
  for (i = 1; i < 4096; i++)
    if (a[i] != ((1 + ((i - 2048) % 16)) << 8))
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" { target vect_condition } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
