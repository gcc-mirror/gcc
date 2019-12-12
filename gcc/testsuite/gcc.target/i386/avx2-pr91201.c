/* PR tree-optimization/91201 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -mno-avx512f" } */
/* { dg-final { scan-assembler "\tvpsadbw\t" } } */

#include "sse2-pr91201.c"
