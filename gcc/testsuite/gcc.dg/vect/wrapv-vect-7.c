/* { dg-require-effective-target vect_int } */
/* { dg-add-options bind_pic_locally } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128

short sa[N];
short sb[N];

int main1 ()
{
  int i;
  
  for (i = 0; i < N; i++)
    {
      sb[i] = 5;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (sb[i] != 5)
        abort ();
    }
  
  for (i = 0; i < N; i++)
    {
      sa[i] = sb[i] + (short)100;
    }

  /* check results:  */
#pragma GCC novector
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

/* Fails for 32-bit targets that don't vectorize PLUS.  */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" } } */
