/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32

int ib[N] __attribute__ ((__aligned__(16))) = 
	{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
short sb[N] __attribute__ ((__aligned__(16))) = 
	{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
char cb[N] __attribute__ ((__aligned__(16))) = 
	{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

int main1 (int n, int * __restrict__ pib, 
	   short * __restrict__ psb, 
	   char * __restrict__ pcb)
{
  int i;
  int ia[N];
  short sa[N];
  char ca[N];

  /* Multiple types with different sizes, used in independent
     computations. Vectorizable. The loads are misaligned.  */
  for (i = 0; i < n; i++)
    {
      ia[i] = pib[i];
      sa[i] = psb[i];
      ca[i] = pcb[i];
    }

  /* check results:  */
  for (i = 0; i < n; i++)
    {
      if (ia[i] != pib[i] 
	  || sa[i] != psb[i] 
	  || ca[i] != pcb[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  main1 (N, ib, sb, cb);
  main1 (N-3, ib, sb, &cb[2]);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_align } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 3 "vect" {xfail vect_no_align } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

