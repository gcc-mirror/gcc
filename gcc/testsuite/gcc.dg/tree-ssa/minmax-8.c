/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt" } */

#include <stdint.h>

uint8_t three_minmax11 (uint8_t xc, uint8_t xm, uint8_t xy) {
        uint8_t  xk;
    if (xc < xm) {
        xk = (uint8_t) (xc > xy ? xc : xy);
    } else {
        xk = (uint8_t) (xm > xy ? xm : xy);
    }
    return xk;
}

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "phiopt1" } } */
