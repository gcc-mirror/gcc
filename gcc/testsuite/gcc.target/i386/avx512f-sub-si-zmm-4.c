/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[^\n\]*%zmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vpsubd\[ \\t\]+%zmm\[0-9\]+, %zmm\[0-9\]+, %zmm0" 1 } } */

#define type __m512i
#define vec 512
#define op sub
#define suffix epi32
#define SCALAR int

#include "avx512-binop-4.h"
