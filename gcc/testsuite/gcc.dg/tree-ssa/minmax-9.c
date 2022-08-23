/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

#include <stdint.h>

uint8_t three_min (uint8_t xc, uint8_t xm, uint8_t xy) {
	uint8_t	 xk;
    xc=~xc;
    xm=~xm;
    xy=~xy;
    if (xc < xm) {
        xk = (uint8_t) (xc < xy ? xc : xy);
    } else {
        xk = (uint8_t) (xm < xy ? xm : xy);
    }
    return xk;
}

/* { dg-final { scan-tree-dump-times "= ~" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "optimized" } } */
