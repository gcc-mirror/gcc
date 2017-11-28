/* { dg-require-effective-target vect_int } */
/* { dg-add-options double_vectors } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 128
#define N (VECTOR_BITS * 2 / 8)
#else
#define N 32
#endif

unsigned int ic[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = 
	{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned int ib[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = 
	{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned short sc[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = 
	{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned short sb[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = 
	{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned char cc[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = 
	{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
unsigned char cb[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = 
	{0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

__attribute__ ((noinline))
int main1 (int n, 
	   unsigned int * __restrict__ pic, unsigned int * __restrict__ pib, 
	   unsigned short * __restrict__ psc, unsigned short * __restrict__ psb,
	   unsigned char * __restrict__ pcc, unsigned char * __restrict__ pcb)
{
  int i;
  unsigned int ia[N];
  unsigned short sa[N];
  unsigned char ca[N];

  /* Multiple types with different sizes, used in independent
     computations. Vectorizable. The loads are misaligned.  */
  for (i = 0; i < n; i++)
    {
      ia[i] = pib[i] + pic[i];
      sa[i] = psb[i] + psc[i];
      ca[i] = pcb[i] + pcc[i];
    }

  /* check results:  */
  for (i = 0; i < n; i++)
    {
      if (ia[i] != pib[i] + pic[i] 
	  || sa[i] != psb[i] + psc[i] 
	  || ca[i] != pcb[i] + pcc[i])
        abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  main1 (N, ic, ib, sc, sb, cc, cb);
  main1 (N-3, ic, ib, &sc[1], sb, cc, &cb[2]);
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail { sparc*-*-* && ilp32 } }} } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 6 "vect" { target { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 6 "vect" { xfail { ! { vect_unaligned_possible && vect_align_stack_vars } } } } } */

