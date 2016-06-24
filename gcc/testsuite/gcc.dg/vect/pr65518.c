#include "tree-vect.h"

extern void abort (void);

typedef struct giga
{
  unsigned int g[16];
} giga;

unsigned long __attribute__((noinline,noclone))
addfst(giga const *gptr, int num)
{
  unsigned int retval = 0;
  int i;
  for (i = 0; i < num; i++)
    retval += gptr[i].g[0];
  return retval;
}

int main ()
{
  struct giga g[8];
  unsigned int n = 1;
  int i, j;
  check_vect ();
  for (i = 0; i < 8; ++i)
    for (j = 0; j < 16; ++j)
      {
	g[i].g[j] = n++;
	__asm__ volatile ("");
      }
  if (addfst (g, 8) != 456)
    abort ();
  return 0;
}

/* We don't want to vectorize the single-element interleaving in the way
   we currently do that (without ignoring not needed vectors in the
   gap between gptr[0].g[0] and gptr[1].g[0]), because that's very
   sub-optimal and causes memory explosion (even though the cost model
   should reject that in the end).  */

/* { dg-final { scan-tree-dump-times "vectorized 0 loops in function" 2 "vect" } } */
