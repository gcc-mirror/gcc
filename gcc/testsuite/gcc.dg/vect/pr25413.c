/* { dg-require-effective-target vect_double } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 8

struct
{
  char c;
  double d[N];
} a;

__attribute__ ((noinline))
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
#pragma GCC novector
  for (i=0; i<N; i++)
    if (a.d[i] != 1)
      abort ();
  return 0;
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vector_alignment_reachable_for_64bit } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { target { {! vector_alignment_reachable_for_64bit} && {! vect_hw_misalign} } } } } */
/* { dg-final { scan-tree-dump-times "vector alignment may not be reachable" 1 "vect" { target { {! vector_alignment_reachable_for_64bit} && {! vect_hw_misalign} } } } } */
/* { dg-final { scan-tree-dump-times "not vectorized: unsupported unaligned store" 1 "vect" { target { {! vector_alignment_reachable_for_64bit} && {! vect_hw_misalign} } } } } */
