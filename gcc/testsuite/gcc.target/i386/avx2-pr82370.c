/* PR target/82370 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -mno-avx512f -masm=att" } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$7, %xmm\[0-9]\+, %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsllq\[ \t]\+\\\$7, %xmm\[0-9]\+, %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$7, %xmm\[0-9]\+, %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrad\[ \t]\+\\\$3, %xmm\[0-9]\+, %xmm\[0-9]\+" 2 } } */
/* { dg-final { scan-assembler-times "vpsraq\[ \t]\+\\\$3, %xmm\[0-9]\+, %xmm\[0-9]\+" 0 } } */
/* { dg-final { scan-assembler-times "vpsraw\[ \t]\+\\\$3, %xmm\[0-9]\+, %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrld\[ \t]\+\\\$5, %xmm\[0-9]\+, %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrlq\[ \t]\+\\\$5, %xmm\[0-9]\+, %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrlw\[ \t]\+\\\$5, %xmm\[0-9]\+, %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$7, %ymm\[0-9]\+, %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsllq\[ \t]\+\\\$7, %ymm\[0-9]\+, %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$7, %ymm\[0-9]\+, %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrad\[ \t]\+\\\$3, %ymm\[0-9]\+, %ymm\[0-9]\+" 2 } } */
/* { dg-final { scan-assembler-times "vpsraq\[ \t]\+\\\$3, %ymm\[0-9]\+, %ymm\[0-9]\+" 0 } } */
/* { dg-final { scan-assembler-times "vpsraw\[ \t]\+\\\$3, %ymm\[0-9]\+, %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrld\[ \t]\+\\\$5, %ymm\[0-9]\+, %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrlq\[ \t]\+\\\$5, %ymm\[0-9]\+, %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrlw\[ \t]\+\\\$5, %ymm\[0-9]\+, %ymm\[0-9]\+" 1 } } */

#include "avx-pr82370.c"
