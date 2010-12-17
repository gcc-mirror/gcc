/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128
#define RES 21888 

/* unaligned store.  */

int ib[N+10];
int ia[N+10];
int ic[N+10];

__attribute__ ((noinline))
int main1 ()
{
  int i, suma = 0, sumb = 0, sumc = 0;

  /* ib and ic have same misalignment, we peel to align them.  */
  for (i = 1; i <= N; i++)
    {
      suma += ia[i];
      sumb += ib[i+6];
      sumc += ic[i+2];
    }

  /* check results:  */
  if (suma + sumb + sumc != RES)
    abort ();

  return 0;
}

int main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N+10; i++)
    {
      asm volatile ("" : "+r" (i));
      ib[i] = i;
      ic[i] = i+2;
      ia[i] = i/2;
    }

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect"  { xfail vect_no_align } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
