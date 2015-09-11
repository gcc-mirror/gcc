/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32

unsigned int ia[N];
unsigned int ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned  int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned short sa[N];
unsigned short sc[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned short sb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned char ca[N];
unsigned char cc[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned char cb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

__attribute__ ((noinline))
int main1 ()
{
  int i;

  /* Multiple types with different sizes, used in independent
     computations. Vectorizable. All accesses aligned.  */
  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] + ic[i];
      sa[i] = sb[i] + sc[i];
      ca[i] = cb[i] + cc[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != ib[i] + ic[i] 
	  || sa[i] != sb[i] + sc[i] 
	  || ca[i] != cb[i] + cc[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail { sparc*-*-* && ilp32 } } } } */

