/* { dg-do run { target powerpc*-*-* } } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -maltivec" { target powerpc*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -msse2" { target i?86-*-* x86_64-*-* } } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

int main1 ()
{
  int i;
  short sa[N];
  short sb[N];
  
  for (i = 0; i < N; i++)
    {
      sb[i] = 5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (sb[i] != 5)
        abort ();
    }
  
  for (i = 0; i < N; i++)
    {
      sa[i] = sb[i] + 100;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (sa[i] != 105)
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
