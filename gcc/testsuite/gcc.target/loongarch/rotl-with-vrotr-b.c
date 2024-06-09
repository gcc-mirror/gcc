/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler-times "vrotr\\.b" 2 } } */
/* { dg-final { scan-assembler-times "vneg\\.b" 1 } } */

#define TYPE char
#include "rotl-with-vrotr-w.c"
