/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param vect-max-peeling-for-alignment=0 -fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 128
#define NINTS (VECTOR_BITS / 32)
#else
#define NINTS 4
#endif

#define N (NINTS * 6)

/* Keep execution time down.  */
#if N <= 24
#define OUTERN N
#else
#define OUTERN NINTS
#endif

struct s{
  int m;
  int n[4][4][N];
};

struct test1{
  struct s a; /* array a.n is unaligned */
  int b;
  int c;
  struct s e[N]; /* array e.n is aligned */
};

/* Avoid big local temporaries.  */
#if NINTS > 8
struct test1 tmp1;
#endif

__attribute__ ((noinline))
int main1 ()
{
  int i,j;
#if NINTS <= 8
  struct test1 tmp1;
#endif

  for (i = 0; i < OUTERN; i++)
    for (j = NINTS - 1; j < N - NINTS + 1; j++)
      {
        tmp1.e[i].n[1][2][j] = 8;
      }

  /* check results:  */
  for (i = 0; i < OUTERN; i++)
#pragma GCC novector
    for (j = NINTS - 1; j < N - NINTS + 1; j++)
    {
      if (tmp1.e[i].n[1][2][j] != 8)
          abort ();
    }
  
  /* not consecutive, will use strided stores */
  for (i = 0; i < OUTERN; i++)
    for (j = NINTS - 1; j < N - NINTS + 1; j++)
      { 
        tmp1.e[j].n[1][2][j] = 8;
      }
  
  /* check results:  */
  for (i = 0; i < OUTERN; i++)
#pragma GCC novector
    for (j = NINTS - 1; j < N - NINTS + 1; j++)
    {
      if (tmp1.e[j].n[1][2][j] != 8)
          abort ();
    }
    
  return 0;
}
       
int main (void)
{
  check_vect ();
    
  return main1 ();
}
          
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" {target {{! vector_alignment_reachable} && {! vect_hw_misalign} } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
