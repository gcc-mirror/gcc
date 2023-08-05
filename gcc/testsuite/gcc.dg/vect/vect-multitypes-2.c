/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32

int ia[N];
int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
short sa[N];
short sb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
char ca[N];
char cb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

__attribute__ ((noinline)) int main1 ()
{
  int i;

  /* Multiple types with different sizes, used in independent
     cmputations. Vectorizable. All accesses aligned.   */
  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i];
      sa[i] = sb[i];
      ca[i] = cb[i];
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (ia[i] != ib[i] 
	  || sa[i] != sb[i] 
	  || ca[i] != cb[i])
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

