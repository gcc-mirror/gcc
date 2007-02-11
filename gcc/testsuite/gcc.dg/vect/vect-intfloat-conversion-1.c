/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32

int main1 ()
{
  int i;
  int ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  float fa[N];

  /* int -> float */
  for (i = 0; i < N; i++)
    {
      fa[i] = (float) ib[i];	
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (fa[i] != (float) ib[i]) 
        abort (); 
    }   

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target powerpc*-*-* i?86-*-* x86_64-*-* } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
