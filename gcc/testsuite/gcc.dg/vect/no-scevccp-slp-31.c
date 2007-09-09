/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include <stdio.h>
#include "tree-vect.h"

#define N 128 

int
main1 ()
{
  int i, j;
  unsigned short out[N*8], a[N][N];
   
 for (i = 0; i < N; i++)
   {
    for (j = 0; j < N; j++)
      {
        a[i][j] = 8;
      }
    out[i*4] = 8;
    out[i*4 + 1] = 18;
    out[i*4 + 2] = 28;
    out[i*4 + 3] = 38;
   }

  /* check results:  */
 for (i = 0; i < N; i++)
   {
    for (j = 0; j < N; j++) 
      {
        if (a[i][j] != 8)
           abort ();
      }
    if (out[i*4] != 8
        || out[i*4 + 1] != 18
        || out[i*4 + 2] != 28
        || out[i*4 + 3] != 38)
      abort();
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
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect"  } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
  
