/* PR tree-optimization/91201 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512bw -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler "\tvpsadbw\t" } } */

#include "sse2-pr91201.c"
