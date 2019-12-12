/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpaddd\[ \\t\]+\\(%(?:eax|rdi|edi)\\)\\\{1to\[1-8\]+\\\}, %zmm\[0-9\]+, %zmm0" 1 } } */
/* { dg-final { scan-assembler-not "vpbroadcastd\[^\n\]*%zmm\[0-9\]+" } } */

#define type __m512i
#define vec 512
#define op add
#define suffix epi32
#define SCALAR int

#include "avx512-binop-1.h"
