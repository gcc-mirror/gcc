#include <stdarg.h>
#include "tree-vect.h"

#define N 128

/* unaligned store.  */

int ib[N+7];

__attribute__ ((noinline))
int main1 ()
{
  int i;
  int ia[N+1];

  /* The store is aligned and the loads are misaligned with the same 
     misalignment. Cost model is disabled. If misaligned stores are supported,
     we peel according to the loads to align them.  */
  for (i = 0; i <= N; i++)
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

