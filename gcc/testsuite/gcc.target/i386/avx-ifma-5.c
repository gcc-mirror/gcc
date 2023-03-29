/* { dg-do compile } */
/* { dg-options "-O2 -mavxifma -mavx512ifma -mavx512vl" } */
/* { dg-final { scan-assembler-times "\{vex\} vpmadd52huq\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "\{vex\} vpmadd52luq\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "\{vex\} vpmadd52huq\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "\{vex\} vpmadd52luq\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+" 1 } } */

#include <immintrin.h>

#include "avx-ifma-1.c"
