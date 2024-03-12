/* { dg-do compile } */
/* { dg-options "-mavx512f -mno-avx512vl -mprefer-vector-width=512 -O2" } */
/* { dg-final { scan-assembler-times "vpternlogd\[ \\t\]+\\\$0xdd, \\(%(?:eax|rdi|edi)\\)\\\{1to\[1-8\]+\\\}, %zmm\[0-9\]+, %zmm0" 1 } } */
/* { dg-final { scan-assembler-not "vpbroadcast" } } */

#define type __m512i
#define vec 512
#define op or
#define suffix epi32
#define SCALAR int

#include "avx512-binop-not-1.h"
