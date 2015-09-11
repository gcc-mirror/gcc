/* { dg-do compile } */
/* { dg-options "-O3 -mavx -mno-avx2 -mtune=generic -mprefer-avx128 -fdump-tree-vect-details" } */

#include "avx-cvt-1.c"

/* { dg-final { scan-tree-dump-times "note: vectorized 1 loops in function" 6 "vect" } } */
/* { dg-final { scan-assembler "vcvttpd2dq(x\[^\n\r\]*%xmm|\[^\n\r\]*xmm\[^\n\r\]*XMMWORD PTR)" } } */
/* { dg-final { scan-assembler "vcvtdq2ps\[^\n\r\]*xmm" } } */
/* { dg-final { scan-assembler "vcvtps2pd\[^\n\r\]*(%xmm\[^\n\r\]*%xmm|xmm\[^\n\r\]*xmm)" } } */
/* { dg-final { scan-assembler "vcvttps2dq\[^\n\r\]*xmm" } } */
/* { dg-final { scan-assembler "vcvtdq2pd\[^\n\r\]*xmm\[^\n\r\]*xmm" } } */
/* { dg-final { scan-assembler "vcvtpd2ps(x\[^\n\r\]*%xmm|\[^\n\r\]*xmm\[^\n\r\]*XMMWORD PTR)" } } */
