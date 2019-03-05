/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpsubd\[ \\t\]+\\(%(?:eax|rdi|edi)\\)\\\{1to\[1-8\]+\\\}, %xmm\[0-9\]+, %xmm0" 1 } } */
/* { dg-final { scan-assembler-not "vpbroadcastd\[^\n\]*%xmm\[0-9\]+" } } */

#define type __m128i
#define vec
#define op sub
#define suffix epi32
#define SCALAR int

#include "avx512-binop-1.h"
