/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

__attribute__ ((noinline)) void
ayuv2yuyv_ref (int *d, int *src, int n)
{
  char *dest = (char *)d;
  int i;

  for(i=0;i<n/2;i++){
    dest[i*4 + 0] = (src[i*2 + 0])>>16;
    dest[i*4 + 1] = (src[i*2 + 1])>>8;
    dest[i*4 + 2] = (src[i*2 + 0])>>16;
    dest[i*4 + 3] = (src[i*2 + 0])>>0;
  }

  /* Check results.  */
#pragma GCC novector
  for(i=0;i<n/2;i++){
   if (dest[i*4 + 0] != (src[i*2 + 0])>>16
       || dest[i*4 + 1] != (src[i*2 + 1])>>8
       || dest[i*4 + 2] != (src[i*2 + 0])>>16
       || dest[i*4 + 3] != (src[i*2 + 0])>>0) 
     abort();
  }
}

int main ()
{
  int d[256], src[128], i;
 
  check_vect ();

  for (i = 0; i < 128; i++)
    src[i] = i; 
  
  ayuv2yuyv_ref(d, src, 128);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target { vect_strided4 && vect_strided2 } } } } */



