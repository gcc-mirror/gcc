/* { dg-additional-options "--param=sccvn-max-alias-queries-per-access=1" } */

#include "tree-vect.h"

extern void abort (void);

typedef struct
{
  int l, h;
} tFPinterval;

tFPinterval X[1024];
tFPinterval Y[1024];
tFPinterval Z[1024];

void __attribute__((noinline))
Compute (void)
{
  int d;
  for (d = 0; d < 1024; d++)
    {
      Y[d].l = X[d].l + X[d].h;
      Y[d].h = Y[d].l;
      Z[d].l = X[d].l;
      Z[d].h = X[d].h;
    }
}

int
main (void)
{
  int d;

  check_vect ();

  for (d = 0; d < 1024; d++)
    {
      X[d].l = d;
      X[d].h = d + 1;
      __asm__ volatile ("");
    }

  Compute ();

  for (d = 0; d < 1024; d++)
    {
      if (Y[d].l != X[d].l + X[d].h
	 || Y[d].h != Y[d].l
	 || Z[d].l != X[d].l
	 || Z[d].h != X[d].h)
	abort ();
      __asm__ volatile ("");
    }

  return 0;
}

