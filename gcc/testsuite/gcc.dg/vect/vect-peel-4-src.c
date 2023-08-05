#include <stdarg.h>
#include "tree-vect.h"

#define N 128

int ib[N+7];
int ia[N+1];

__attribute__ ((noinline))
int main1 ()
{
  int i;

  /* Don't peel keeping one load and the store aligned.  */
  for (i = 0; i <= N; i++)
    {
      ia[i] = ib[i] + ib[i+5];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 1; i <= N; i++)
    {
      if (ia[i] != ib[i] + ib[i+5])
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


