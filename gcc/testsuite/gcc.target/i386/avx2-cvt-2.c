/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -mtune=generic -fdump-tree-vect-details" } */

#include "avx2-cvt-1.c"

/* { dg-final { scan-tree-dump-times "note: vectorized 1 loops in function" 6 "vect" } } */
/* { dg-final { scan-assembler "vcvttpd2dq(y\[^\n\r\]*%xmm|\[^\n\r\]*xmm\[^\n\r\]*YMMWORD PTR)" } } */
/* { dg-final { scan-assembler "vcvtdq2ps\[^\n\r\]*ymm" } } */
/* { dg-final { scan-assembler "vcvtps2pd\[^\n\r\]*(%xmm\[^\n\r\]*%ymm|ymm\[^\n\r\]*xmm)" } } */
/* { dg-final { scan-assembler "vcvttps2dq\[^\n\r\]*ymm" } } */
/* { dg-final { scan-assembler "vcvtdq2pd\[^\n\r\]*(%xmm\[^\n\r\]*%ymm|ymm\[^\n\r\]*xmm)" } } */
/* { dg-final { scan-assembler "vcvtpd2ps(y\[^\n\r\]*%xmm|\[^\n\r\]*xmm\[^\n\r\]*YMMWORD PTR)" } } */
