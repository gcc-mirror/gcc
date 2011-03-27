/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-load" } */

#include "avx-check.h"

#define N 128

char **ep;
char **fp;
char **mp;
char **lp;

__attribute__ ((noinline))
void
foo (void)
{
  mp = (char **) malloc (N);
  lp = (char **) malloc (N);
  ep = (char **) malloc (N);
  fp = (char **) malloc (N);
}

void
avx_test (void)
{
  int i;
  char **ap, **bp, **cp, **dp;
  char *str = "STR";

  foo ();

  cp = mp;
  dp = lp;

  for (i = N; i >= 0; i--)
    {
      *cp++ = str;
      *dp++ = str;
    }

  ap = ep;
  bp = fp;
  cp = mp;
  dp = lp;

  for (i = N; i >= 0; i--)
    {
      *ap++ = *cp++;
      *bp++ = *dp++;
    }

  for (i = N; i >= 0; i--)
    {
      if (strcmp (*--ap, "STR") != 0)
	abort ();
      if (strcmp (*--bp, "STR") != 0)
	abort ();
    }
}
