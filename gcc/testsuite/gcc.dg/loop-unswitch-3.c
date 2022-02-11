/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-all" } */

#include <stdlib.h>
#define N 32
float *foo(int ustride, int size, float *src)
{
   float *buffer, *p;
   int i, k;

   if (!src)
    return NULL;

   buffer = (float *) malloc(N * size * sizeof(float));

   if(buffer)
      for(i=0, p=buffer; i<N; i++, src+=ustride)
	for(k=0; k<size; k++)
	  *p++ = src[k];

   return buffer;
}

/* { dg-final { scan-tree-dump-times "Guard hoisted" 1 "unswitch" } } */
