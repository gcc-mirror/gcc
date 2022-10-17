/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-load" } */

#include "avx-check.h"

#define N 128

char **ep;
char **fp;
char **mp;
char **lp;
extern int strcmp (const char *, const char *);

__attribute__ ((noinline))
void
foo (void)
{
  mp = (char **) malloc (N * sizeof (char **));
  lp = (char **) malloc (N * sizeof (char **));
  ep = (char **) malloc (N * sizeof (char **));
  fp = (char **) malloc (N * sizeof (char **));
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

  for (i = N; i > 0; i--)
    {
      *cp++ = str;
      *dp++ = str;
    }

  ap = ep;
  bp = fp;
  cp = mp;
  dp = lp;

  for (i = N; i > 0; i--)
    {
      *ap++ = *cp++;
      *bp++ = *dp++;
    }

  for (i = N; i > 0; i--)
    {
      if (strcmp (*--ap, "STR") != 0)
	abort ();
      if (strcmp (*--bp, "STR") != 0)
	abort ();
    }
}
