/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler-times "vrotr\\.h" 2 } } */
/* { dg-final { scan-assembler-times "vneg\\.h" 1 } } */

#define TYPE short
#include "rotl-with-vrotr-w.c"
