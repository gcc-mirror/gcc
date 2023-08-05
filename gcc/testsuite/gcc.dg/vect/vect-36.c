/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
 
__attribute__ ((noinline))
int main1 ()
{  
  int i;
  struct {
    char ca[N];
    char cb[N];
  } s;


  for (i = 0; i < N; i++)
    {
      s.cb[i] = 3*i;
      __asm__ volatile ("");
    }

  for (i = 0; i < N; i++)
    {
      s.ca[i] = s.cb[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (s.ca[i] != s.cb[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
