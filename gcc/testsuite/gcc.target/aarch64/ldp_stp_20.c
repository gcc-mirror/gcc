/* { dg-options "-O2" } */

#pragma GCC target "+nosimd+fp"

#include "ldp_stp_6.c"

/* { dg-final { scan-assembler "stp\td\[0-9\]+, d\[0-9\]+, \\\[x\[0-9\]+\\\]" } } */
