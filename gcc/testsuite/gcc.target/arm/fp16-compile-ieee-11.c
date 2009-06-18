/* { dg-do compile } */
/* { dg-options "-mfp16-format=ieee -std=gnu99" } */

#include <math.h>

/* 0x7c00 = 31744 */
__fp16 xx = INFINITY;

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t31744" } } */
