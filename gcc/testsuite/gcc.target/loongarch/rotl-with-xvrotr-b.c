/* { dg-do compile } */
/* { dg-options "-O2 -mlasx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler-times "xvrotr\\.b" 2 } } */
/* { dg-final { scan-assembler-times "xvneg\\.b" 1 } } */

#define VLEN 32
#include "rotl-with-vrotr-b.c"
