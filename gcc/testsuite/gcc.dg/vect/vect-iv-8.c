/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 26
 
__attribute__ ((noinline)) int main1 (short X)
{  
  unsigned char a[N];
  unsigned short b[N];
  unsigned int c[N];
  short myX = X;
  int i;

  /* vectorization of induction with type conversions.  */
  for (i = 0; i < N; i++)
  {
    a[i] = (unsigned char)X;
    b[i] = X;
    c[i] = (unsigned int)X;
    X++;
  }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (a[i] != (unsigned char)myX || b[i] != myX || c[i] != (unsigned int)myX++)
	abort ();
    }

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 (3);
} 

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_pack_trunc && vect_unpack } } } } */
