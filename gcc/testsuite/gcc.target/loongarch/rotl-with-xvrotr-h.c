/* { dg-do compile } */
/* { dg-options "-O2 -mlasx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler-times "xvrotr\\.h" 2 } } */
/* { dg-final { scan-assembler-times "xvneg\\.h" 1 } } */

#define VLEN 32
#include "rotl-with-vrotr-h.c"
