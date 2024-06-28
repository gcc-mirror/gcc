/* { dg-do compile } */
/* { dg-options "-mavx512f -mno-avx512vl -mprefer-vector-width=512 -O2" } */
/* { dg-final { scan-assembler-times "vpternlogq\[ \\t\]+\\\$80, \\(%(?:eax|rdi|edi)\\)\\\{1to\[1-8\]+\\\}, %zmm\[0-9\]+, %zmm0" 1 } } */
/* { dg-final { scan-assembler-not "vpbroadcast" } } */

#define type __m512i
#define vec 512
#define op andnot
#define suffix epi64
#define SCALAR long long

#include "avx512-binop-2.h"
