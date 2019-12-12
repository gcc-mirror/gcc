/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpaddd\[ \\t\]+\\(%(?:eax|rdi|edi)\\)\\\{1to\[1-8\]+\\\}, %ymm\[0-9\]+, %ymm0" 1 } } */
/* { dg-final { scan-assembler-not "vpbroadcastd\[^\n\]*%ymm\[0-9\]+" } } */

#define type __m256i
#define vec 256
#define op add
#define suffix epi32
#define SCALAR int

#include "avx512-binop-1.h"
