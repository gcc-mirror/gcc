/* { dg-do compile } */
/* { dg-options "-O2 -mlasx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler-times "xvrotr\\.d" 2 } } */
/* { dg-final { scan-assembler-times "xvneg\\.d" 1 } } */

#define VLEN 32
#include "rotl-with-vrotr-d.c"
