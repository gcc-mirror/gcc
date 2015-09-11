/* { dg-require-effective-target vect_double } */

#include "tree-vect.h"

extern long int lrint (double);
extern void abort (void);
long int a[64];
double b[64];

__attribute__((noinline, noclone)) void
f1 (void)
{
  a[0] = lrint (b[0]) + 1;
  a[1] = lrint (b[1]) + 2;
  a[2] = lrint (b[2]) + 3;
  a[3] = lrint (b[3]) + 4;
  a[4] = lrint (b[4]) + 5;
  a[5] = lrint (b[5]) + 6;
  a[6] = lrint (b[6]) + 7;
  a[7] = lrint (b[7]) + 8;
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  a[0] = lrint (b[0]);
  a[1] = lrint (b[1]);
  a[2] = lrint (b[2]);
  a[3] = lrint (b[3]);
  a[4] = lrint (b[4]);
  a[5] = lrint (b[5]);
  a[6] = lrint (b[6]);
  a[7] = lrint (b[7]);
}

__attribute__((noinline, noclone)) int
main1 ()
{
  int i;

  for (i = 0; i < 8; i++)
    {
      asm ("");
      b[i] = ((i & 1) ? -4 * i : 4 * i) + 0.25;
    }
  f1 ();
  for (i = 0; i < 8; i++)
    if (a[i] != ((i & 1) ? -4 * i : 4 * i) + 1 + i)
      abort ();
    else
      a[i] = 131.25;
  f2 ();
  for (i = 0; i < 8; i++)
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

/* { dg-final { scan-tree-dump-times "basic block vectorized" 2 "slp2" { target vect_call_lrint } } } */
