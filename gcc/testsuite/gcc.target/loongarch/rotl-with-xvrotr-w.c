/* { dg-do compile } */
/* { dg-options "-O2 -mlasx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler-times "xvrotr\\.w" 2 } } */
/* { dg-final { scan-assembler-times "xvneg\\.w" 1 } } */

#define VLEN 32
#include "rotl-with-vrotr-w.c"
