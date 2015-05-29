/* { dg-skip-if "" { vect_no_align } } */
/* { dg-require-effective-target vect_int } */
/* { dg-add-options bind_pic_locally } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32

short sa[N];
short sc[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
		16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
short sb[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
		16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
int ia[N];
int ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,
	       0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,
	       0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

__attribute__ ((noinline)) 
int main1 (int n)
{
  int i;

  /* Multiple types with different sizes, used in idependent
     copmutations. Vectorizable.  */
  for (i = 0; i < n; i++)
    {
      sa[i+2] = sb[i] + sc[i];
      ia[i+1] = ib[i] + ic[i];
    }

  /* check results:  */
  for (i = 0; i < n; i++)
    {
      if (sa[i+2] != sb[i] + sc[i] || ia[i+1] != ib[i] + ic[i])
	abort ();
    }

  return 0;
}

int main2 (int n)
{
  int i;

  /* Multiple types with different sizes, used in idependent
     copmutations. Vectorizable.  */
  for (i = 0; i < n; i++)
    {
      ia[i+1] = ib[i] + ic[i];
      sa[i] = sb[i] + sc[i];
    }

  /* check results:  */
  for (i = 0; i < n; i++)
    {
      if (sa[i] != sb[i] + sc[i] || ia[i+1] != ib[i] + ic[i])
        abort ();
    }

  return 0;
}


int main (void)
{ 
  check_vect ();
  
  main1 (N-2);
  main2 (N-1);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_element_align } } } */
/* { dg-final { scan-tree-dump-times "not vectorized: unsupported unaligned store" 2 "vect" { xfail vect_element_align } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 3 "vect" { target vect_element_align } } } */

