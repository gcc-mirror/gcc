/* { dg-options "-O2" } */

#pragma GCC target "+nosimd+fp"

#include "ldp_stp_8.c"

/* { dg-final { scan-assembler-times "ldp\td\[0-9\], d\[0-9\]+, \\\[x\[0-9\]+\\\]" 2 } } */
