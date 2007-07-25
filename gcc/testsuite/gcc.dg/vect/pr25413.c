/* { dg-require-effective-target vect_double } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 8

struct
{
  char c;
  double d[N];
} a;

int main1()
{
  int i;
  for ( i=0; i<N; ++i )
    a.d[i]=1;
  return 0;
}

int main (void)
{ 
  int i;
  check_vect ();
  
  main1 ();
  for (i=0; i<N; i++)
    if (a.d[i] != 1)
      abort ();
  return 0;
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_aligned_arrays } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { target { ! vect_aligned_arrays } } } } */
/* { dg-final { scan-tree-dump-times "vector alignment may not be reachable" 1 "vect" { target { ! vect_aligned_arrays } } } } */
/* { dg-final { scan-tree-dump-times "not vectorized: unsupported unaligned store" 1 "vect" { target { ! vect_aligned_arrays } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
