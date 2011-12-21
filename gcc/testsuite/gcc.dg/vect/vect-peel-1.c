/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

int ib[N+7];

__attribute__ ((noinline))
int main1 ()
{
  int i;
  int ia[N+1];

  /* All the accesses are misaligned. With cost model disabled, we
     count the number of aligned accesses for each peeling option, and
     in this case we align the two loads if possible (i.e., if
     misaligned stores are supported).  */
  for (i = 1; i <= N; i++)
    {
      ia[i] = ib[i+2] + ib[i+6];
    }

  /* check results:  */
  for (i = 1; i <= N; i++)
    {
      if (ia[i] != ib[i+2] + ib[i+6])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  int i;

  check_vect ();

  for (i = 0; i <= N+6; i++)
    {
      asm volatile ("" : "+r" (i));
      ib[i] = i;
    }

  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { target { { vect_element_align } && { vect_aligned_arrays } } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
