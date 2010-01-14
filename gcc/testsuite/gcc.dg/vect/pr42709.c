/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

int *res[N];

int
main1 (int *a, int *b, int *c, int *d, int dummy)
{
  int i;

  for (i = 0; i < N/2; i+=4)
    {
      res[i] = a + 16;
      res[i+1] = b + 16;
      res[i+2] = c + 16;
      res[i+3] = d + 16;
      if (dummy == 32)
        abort ();
    } 
}

/* { dg-final { cleanup-tree-dump "vect" } } */
  
