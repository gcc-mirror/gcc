/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32

short sa[N];
short sb[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
		16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};
int ia[N];
int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,
	       0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};

/* Current peeling-for-alignment scheme will consider the 'sa[i+7]'
   access for peeling, and therefore will examine the option of
   using a peeling factor = V-7%V = 1,3 for V=8,4 respectively, 
   which will also align the access to 'ia[i+3]', and the loop could be 
   vectorized on all targets that support unaligned loads.  */

__attribute__ ((noinline)) int main1 (int n)
{
  int i;

  /* Multiple types with different sizes, used in idependent
     copmutations. Vectorizable.  */
  for (i = 0; i < n; i++)
    {
      sa[i+7] = sb[i];
      ia[i+3] = ib[i];
    }

  /* check results:  */
  for (i = 0; i < n; i++)
    {
      if (sa[i+7] != sb[i] || ia[i+3] != ib[i])
	abort ();
    }

  return 0;
}

/* Current peeling-for-alignment scheme will consider the 'ia[i+3]'
   access for peeling, and therefore will examine the option of
   using a peeling factor = (V-3)%V = 1 for V=2,4. 
   This will not align the access 'sa[i+3]' (for which we need to
   peel 5 iterations), so the loop can not be vectorized.  */

__attribute__ ((noinline)) int main2 (int n)
{
  int i;

  /* Multiple types with different sizes, used in independent
     copmutations.  */
  for (i = 0; i < n; i++)
    {
      ia[i+3] = ib[i];
      sa[i+3] = sb[i];
    }

  /* check results:  */
  for (i = 0; i < n; i++)
    {
      if (sa[i+3] != sb[i] || ia[i+3] != ib[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  main1 (N-7);
  main2 (N-3);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { xfail {! vect_hw_misalign}  } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail { vect_no_align || vect_hw_misalign } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 2 "vect" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail { vect_no_align || vect_hw_misalign }} } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 4 "vect" { xfail {! vect_hw_misalign} } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { xfail { vect_no_align || vect_hw_misalign } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

