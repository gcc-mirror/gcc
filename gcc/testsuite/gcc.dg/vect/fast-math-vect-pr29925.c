/* { dg-require-effective-target vect_float } */

#include <stdlib.h>
#include "tree-vect.h"

void interp_pitch(float *exc, float *interp, int pitch, int len)
{
   int i,k;
   int maxj;

   maxj=3;
   for (i=0;i<len;i++)
   {
      float tmp = 0;
      for (k=0;k<7;k++)
      {
         tmp += exc[i-pitch+k+maxj-6];
      }
      interp[i] = tmp;
   }
}

int main()
{
   float *exc = calloc(126,sizeof(float));
   float *interp = calloc(80,sizeof(float));
   int pitch = -35;

   check_vect ();

   interp_pitch(exc, interp, pitch, 80);
   free(exc);
   free(interp);
   return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

