#include "tree-vect.h"

extern long int lrint (double);
extern void abort (void);
long int a[64];
double b[64];

__attribute__((noinline, noclone)) void
f1 (int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      a[4 * i + 0] = lrint (b[4 * i + 0]) + 1;
      a[4 * i + 1] = lrint (b[4 * i + 1]) + 2;
      a[4 * i + 2] = lrint (b[4 * i + 2]) + 3;
      a[4 * i + 3] = lrint (b[4 * i + 3]) + 4;
    }
}

__attribute__((noinline, noclone)) void
f2 (int n)
{
  int i;
  for (i = 0; i < 2 * n; i++)
    {
      a[2 * i + 0] = lrint (b[2 * i + 0]) + 1;
      a[2 * i + 1] = lrint (b[2 * i + 1]) + 2;
    }
}

__attribute__((noinline, noclone)) void
f3 (void)
{
  int i;
  for (i = 0; i < 64; i++)
    a[i] = lrint (b[i]) + 1;
}

__attribute__((noinline, noclone)) void
f4 (int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      a[4 * i + 0] = lrint (b[4 * i + 0]);
      a[4 * i + 1] = lrint (b[4 * i + 1]);
      a[4 * i + 2] = lrint (b[4 * i + 2]);
      a[4 * i + 3] = lrint (b[4 * i + 3]);
    }
}

__attribute__((noinline, noclone)) void
f5 (int n)
{
  int i;
  for (i = 0; i < 2 * n; i++)
    {
      a[2 * i + 0] = lrint (b[2 * i + 0]);
      a[2 * i + 1] = lrint (b[2 * i + 1]);
    }
}

__attribute__((noinline, noclone)) void
f6 (void)
{
  int i;
  for (i = 0; i < 64; i++)
    a[i] = lrint (b[i]);
}

__attribute__((noinline, noclone)) int
main1 ()
{
  int i;

  for (i = 0; i < 64; i++)
    {
      asm ("");
      b[i] = ((i & 1) ? -4 * i : 4 * i) + 0.25;
    }
  f1 (16);
  for (i = 0; i < 64; i++)
    if (a[i] != ((i & 1) ? -4 * i : 4 * i) + 1 + (i & 3))
      abort ();
    else
      a[i] = 131.25;
  f2 (16);
  for (i = 0; i < 64; i++)
    if (a[i] != ((i & 1) ? -4 * i : 4 * i) + 1 + (i & 1))
      abort ();
    else
      a[i] = 131.25;
  f3 ();
  for (i = 0; i < 64; i++)
    if (a[i] != ((i & 1) ? -4 * i : 4 * i) + 1)
      abort ();
    else
      a[i] = 131.25;
  f4 (16);
  for (i = 0; i < 64; i++)
    if (a[i] != ((i & 1) ? -4 * i : 4 * i))
      abort ();
    else
      a[i] = 131.25;
  f5 (16);
  for (i = 0; i < 64; i++)
    if (a[i] != ((i & 1) ? -4 * i : 4 * i))
      abort ();
    else
      a[i] = 131.25;
  f6 ();
  for (i = 0; i < 64; i++)
    if (a[i] != ((i & 1) ? -4 * i : 4 * i))
      abort ();
  return 0;
}

int
main ()
{
  check_vect ();
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 6 "vect" { target vect_call_lrint } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 4 "vect" { target vect_call_lrint } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
