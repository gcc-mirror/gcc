/* { dg-do run { target powerpc*-*-* } } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-do run { target mipsisa64*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -maltivec" { target powerpc*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -msse2" { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -mpaired-single" { target mipsisa64*-*-* } } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
#define DIFF 240

int main1 ()
{
  int i;
  float b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  float c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  float diff;

  /* Not vetorizable yet (reduction).  */
  diff = 0;
  for (i = 0; i < N; i++) {
    diff += (b[i] - c[i]);
  }

  /* check results:  */
  if (diff != DIFF)
    abort ();

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */
