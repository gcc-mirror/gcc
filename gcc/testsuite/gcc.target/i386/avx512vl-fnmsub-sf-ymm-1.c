/* { dg-do compile } */
/* { dg-options "-mfma -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vfnmsub...ps\[ \\t\]+\\(%(?:eax|rdi|edi)\\)\\\{1to\[1-8\]+\\\}, %ymm\[0-9\]+, %ymm0" 1 } } */
/* { dg-final { scan-assembler-not "vbroadcastss\[^\n\]*%ymm\[0-9\]+" } } */

#define type __m256
#define vec 256
#define op fnmsub
#define suffix ps
#define SCALAR float

#include "avx512-fma-1.h"
