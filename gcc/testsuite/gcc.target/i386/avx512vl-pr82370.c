/* PR target/82370 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mno-avx512bw -masm=att" } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$7, %xmm\[0-9]\+, %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsraw\[ \t]\+\\\$3, %xmm\[0-9]\+, %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrlw\[ \t]\+\\\$5, %xmm\[0-9]\+, %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$7, %ymm\[0-9]\+, %ymm\[0-9]\+" 3 } } */
/* { dg-final { scan-assembler-times "vpsraw\[ \t]\+\\\$3, %ymm\[0-9]\+, %ymm\[0-9]\+" 3 } } */
/* { dg-final { scan-assembler-times "vpsrlw\[ \t]\+\\\$5, %ymm\[0-9]\+, %ymm\[0-9]\+" 3 } } */
/* { dg-final { scan-assembler-times "vps\[lr]\[la]\[dq]\[ \t]\+\\\$\[357], %\[xyz]mm\[0-9]\+, %\[xyz]mm\[0-9]\+" 0 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$7, \\(%\[a-z0-9,]*\\), %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsllq\[ \t]\+\\\$7, \\(%\[a-z0-9,]*\\), %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vps\[lr]\[la]w\[ \t]\+\\\$\[357], \\(%\[a-z0-9,]*\\), %\[xyz]mm\[0-9]\+" 0 } } */
/* { dg-final { scan-assembler-times "vpsrad\[ \t]\+\\\$3, \\(%\[a-z0-9,]*\\), %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsraq\[ \t]\+\\\$3, \\(%\[a-z0-9,]*\\), %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrld\[ \t]\+\\\$5, \\(%\[a-z0-9,]*\\), %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrlq\[ \t]\+\\\$5, \\(%\[a-z0-9,]*\\), %xmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$7, \\(%\[a-z0-9,]*\\), %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsllq\[ \t]\+\\\$7, \\(%\[a-z0-9,]*\\), %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrad\[ \t]\+\\\$3, \\(%\[a-z0-9,]*\\), %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsraq\[ \t]\+\\\$3, \\(%\[a-z0-9,]*\\), %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrld\[ \t]\+\\\$5, \\(%\[a-z0-9,]*\\), %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrlq\[ \t]\+\\\$5, \\(%\[a-z0-9,]*\\), %ymm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$7, \\(%\[a-z0-9,]*\\), %zmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsllq\[ \t]\+\\\$7, \\(%\[a-z0-9,]*\\), %zmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrad\[ \t]\+\\\$3, \\(%\[a-z0-9,]*\\), %zmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsraq\[ \t]\+\\\$3, \\(%\[a-z0-9,]*\\), %zmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrld\[ \t]\+\\\$5, \\(%\[a-z0-9,]*\\), %zmm\[0-9]\+" 1 } } */
/* { dg-final { scan-assembler-times "vpsrlq\[ \t]\+\\\$5, \\(%\[a-z0-9,]*\\), %zmm\[0-9]\+" 1 } } */

#include "avx-pr82370.c"
