/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 256

extern int a[N];

/* The alignment of 'pa' is unknown. 
   Yet we do know that both the read access and write access have 
   the same alignment. Peeling to align one of the accesses will 
   align the other.

   Not vectorized yet due to problems in dataref analysis that
   are fixed in autovect-branch but not yet in mainline.  */

int
main1 (int * pa)
{
  int i;

  for (i = 0; i < N; i++)
    {
      pa[i] = pa[i] + 1;
    }

  return 0;
}

/* The alignment of 'a' is unknown. 
   Yet we do know that both the read access and write access have 
   the same alignment. Peeling to align one of the accesses will 
   align the other.  */

int
main2 ()
{
  int i;

  for (i = 0; i < N; i++)
    {
      a[i] = a[i] + 1;
    }

  return 0;
}

int 
main3 ()
{
  int i;

  for (i = 0; i < N; i++)
    {
      a[i] = a[i+20];
    }

  return 0;
}

/* Currently only the loops in main2 and main3 get vectorized. After the merge 
   of the datarefs-analysis cleanups from autovect-branch to mainline, the loop
   in main1 will also be vectorized.  */ 
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "accesses have the same alignment." 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 2 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
