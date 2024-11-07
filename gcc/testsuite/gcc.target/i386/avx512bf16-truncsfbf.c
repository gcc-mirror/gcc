/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512bf16 -O2 -ffast-math" } */
/* { dg-final { scan-assembler-times {(?n)vcvtneps2bf16} 6 } } */

#include "avx512bw-truncsfbf.c"
