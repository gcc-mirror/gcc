/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-S -O2 -ftree-vectorize -fdump-tree-vect-stats -maltivec" { target powerpc*-*-* } } */
/* { dg-options "-S -O2 -ftree-vectorize -fdump-tree-vect-stats -msse2" { target i?86-*-* x86_64-*-* } } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16
struct test {
  char ca[N];
};

extern struct test s;
 
int main1 ()
{  
  int i;

  for (i = 0; i < N; i++)
    {
      s.ca[i] = 5;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (s.ca[i] != 5)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  return main1 ();
} 


/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
