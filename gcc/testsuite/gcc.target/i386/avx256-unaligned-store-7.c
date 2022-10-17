/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -dp -mavx -mavx256-split-unaligned-store" } */

#include "avx-check.h"

#define N 128

char **ep;
char **fp;
extern int strcmp (const char *, const char *);

__attribute__ ((noinline))
void
foo (void)
{
  ep = (char **) malloc (N * sizeof (char **));
  fp = (char **) malloc (N * sizeof (char **));
}

void
avx_test (void)
{
  int i;
  char **ap, **bp;
  char *str = "STR";

  foo ();

  ap = ep;
  bp = fp;

  for (i = N; i > 0; i--)
    {
      *ap++ = str;
      *bp++ = str;
    }

  for (i = N; i > 0; i--)
    {
      if (strcmp (*--ap, "STR") != 0)
	abort ();
      if (strcmp (*--bp, "STR") != 0)
	abort ();
    }
}
