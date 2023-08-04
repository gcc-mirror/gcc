/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

int
main1 ()
{
  int i, j;
  unsigned short out[N*8], a[N];
   
 for (j = 0; j < N; j++)
   {
    for (i = 0; i < N; i++)
      {
        out[i*4] = 8;
        out[i*4 + 1] = 18;
        out[i*4 + 2] = 28;
        out[i*4 + 3] = 38;
      }
    a[j] = 8;
   }

  /* check results:  */
#pragma GCC novector
   for (j = 0; j < N; j++)
   {
    for (i = 0; i < N; i++)
      {
        if (out[i*4] != 8
            || out[i*4 + 1] != 18
            || out[i*4 + 2] != 28
            || out[i*4 + 3] != 38)
          abort();
      }
   
    if (a[j] != 8)
       abort ();
   }

  return 0;
}

int main (void)
{
  check_vect ();

  main1 ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" } } */
  
