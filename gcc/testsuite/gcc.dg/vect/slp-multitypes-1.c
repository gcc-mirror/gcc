/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128 

__attribute__ ((noinline)) int
main1 ()
{
  int i;
  unsigned short sout[N*8];
  unsigned int iout[N*8];

  for (i = 0; i < N; i++)
    {
      sout[i*4] = 8;
      sout[i*4 + 1] = 18;
      sout[i*4 + 2] = 28;
      sout[i*4 + 3] = 38;

      iout[i*4] = 8;
      iout[i*4 + 1] = 18;
      iout[i*4 + 2] = 28;
      iout[i*4 + 3] = 38;
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (sout[i*4] != 8 
         || sout[i*4 + 1] != 18
         || sout[i*4 + 2] != 28
         || sout[i*4 + 3] != 38
         || iout[i*4] != 8
         || iout[i*4 + 1] != 18
         || iout[i*4 + 2] != 28
         || iout[i*4 + 3] != 38)
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
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect"  } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
  
