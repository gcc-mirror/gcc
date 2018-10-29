/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastss\[^\n\]*%zmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vsubps\[^\n\]*%zmm\[0-9\]+" 1 } } */

#define type __m512
#define vec 512
#define op sub
#define suffix ps
#define SCALAR float

#include "avx512-binop-6.h"
