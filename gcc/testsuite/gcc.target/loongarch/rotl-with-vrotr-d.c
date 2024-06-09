/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler-times "vrotr\\.d" 2 } } */
/* { dg-final { scan-assembler-times "vneg\\.d" 1 } } */

#define TYPE long long
#include "rotl-with-vrotr-w.c"
