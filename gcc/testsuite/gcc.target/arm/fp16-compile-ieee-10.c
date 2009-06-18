/* { dg-do compile } */
/* { dg-options "-mfp16-format=ieee -std=gnu99" } */

#include <math.h>

/* 0x7e00 = 32256 */
__fp16 xx = NAN;

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t32256" } } */
