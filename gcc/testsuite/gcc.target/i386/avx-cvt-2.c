/* { dg-do compile } */
/* { dg-options "-O3 -mavx -mno-avx2 -mtune=generic -fdump-tree-vect-details" } */

#include "avx-cvt-1.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 6 "vect" } } */
/* { dg-final { scan-assembler "vcvttpd2dq" } } */
/* { dg-final { scan-assembler "vcvtdq2ps" } } */
/* { dg-final { scan-assembler "vcvtps2pd" } } */
/* { dg-final { scan-assembler "vcvttps2dq" } } */
/* { dg-final { scan-assembler "vcvtdq2pd" } } */
/* { dg-final { scan-assembler "vcvtpd2ps" } } */
