/* { dg-do compile } */
/* { dg-options "-mavx512f -mno-avx512bw -O2" } */

#include "vect-bfloat16-2a.c"

/* { dg-final { scan-assembler-times "vpunpcklwd" 28 } } */
/* { dg-final { scan-assembler-times "vpunpckldq" 14 } } */
/* { dg-final { scan-assembler-times "vpunpcklqdq" 7 } } */

/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$8" 1 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$6" 1 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$14" 1 } } */
/* { dg-final { scan-assembler-times "vextract" 2 } } */

/* { dg-final { scan-assembler-times "vpbroadcastw" 7 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpblendw" 4 { target { ! ia32 } } } } */

/* { dg-final { scan-assembler-times "vpbroadcastw" 6 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vpblendw" 3 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vpinsrw" 68 { target ia32 } } } */

/* { dg-final { scan-assembler-times "vpblendd" 3 } } */
