/* { dg-do run { target { powerpc*-*-* && lp64 } } } */
/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-additional-options "-mpowerpc64 -maltivec -fdump-tree-optimized-details-blocks" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-skip-if "" { powerpc-ibm-aix* } } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
 
__attribute__ ((noinline))
int main1 ()
{  
  long long unsigned int ca[N];
  int i;

  for (i = 0; i < N; i++)
    {
      ca[i] = 2;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (ca[i] != 2)
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
