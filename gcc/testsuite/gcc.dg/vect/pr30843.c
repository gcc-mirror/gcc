/* { dg-do compile } */
/* { dg-require-effective-target vect_long } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 16

void dacP98FillRGBMap (unsigned char *pBuffer)
{
    unsigned long dw, dw1;
    unsigned long *pdw = (unsigned long *)(pBuffer);

    for( dw = 256, dw1 = 0; dw; dw--, dw1 += 0x01010101) 
    {
       *pdw++ = dw1;
       *pdw++ = dw1;
       *pdw++ = dw1;
       *pdw++ = dw1;
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_interleave || vect_strided4 } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

