/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -Ofast -mprefer-vector-width=128" } */
/* { dg-final { scan-assembler-times "vcvttph2dq" "2" } } */
/* { dg-final { scan-assembler-times "vcvtdq2ph" "2" } } */
/* { dg-final { scan-assembler-times "vcvtph2ps" "2" } } */
/* { dg-final { scan-assembler-times "vcvtps2ph" "2" } } */

#include "vec_pack_fp16-1.c"
