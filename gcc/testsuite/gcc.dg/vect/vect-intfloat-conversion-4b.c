/* { dg-require-effective-target vect_float } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 32

__attribute__ ((noinline)) int main1 ()
{
  int i;
  unsigned short usb[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,0,65533,65530,65527,65524,65521,65518,65515,65512,65509,65506,65503,65500,65497,65494,65491};
  float fa[N];

  /* unsigned short -> float */
  for (i = 0; i < N; i++)
    {
      fa[i] = (float) usb[i];	
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (fa[i] != (float) usb[i]) 
        abort (); 
    }   

  return 0;
}

int main (void)
{ 
  check_vect ();
  
  return main1 ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_intfloat_cvt } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
