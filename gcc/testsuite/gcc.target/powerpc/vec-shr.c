/* { dg-do run } */
/* { dg-options "-O3 -fno-inline" } */

#include <stdlib.h>

typedef struct { double r, i; } complex;
#define LEN 30
complex c[LEN];
double d[LEN];

void
foo (complex *c, double *d, int len1)
{
  int i;
  for (i = 0; i < len1; i++)
    {
      c[i].r = d[i];
      c[i].i = 0.0;
    }
}

int
main (void)
{
  int i;
  for (i = 0; i < LEN; i++)
    d[i] = (double) i;
  foo (c, d, LEN);
  for (i=0;i<LEN;i++)
    if ((c[i].r != (double) i) || (c[i].i != 0.0))
      abort ();
  return 0;
}

