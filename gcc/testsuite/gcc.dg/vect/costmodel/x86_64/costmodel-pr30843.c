/* { dg-do compile } */
/* { dg-require-effective-target vect_long } */

#include <stdarg.h>
#include "../../tree-vect.h"

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

/* Even with SSE2 we should only generate one IV for the induction.  */
/* { dg-final { scan-tree-dump-times "# vect_vec_iv" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
