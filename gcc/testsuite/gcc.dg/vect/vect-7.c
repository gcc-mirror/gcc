/* { dg-require-effective-target vect_int } */

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

/* Fails for targets that don't vectorize PLUS.  */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { xfail alpha*-*-* } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
