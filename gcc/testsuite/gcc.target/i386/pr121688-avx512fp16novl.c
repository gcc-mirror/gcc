/* PR target/121688 */
/* { dg-do compile } */
/* { dg-options "-O2 -mf16c -mavx512fp16 -mno-avx512vl -masm=att" } */
/* { dg-final { scan-assembler-times "\tvcvtps2ph\t\\\$4, %xmm\[0-9]\+, %xmm\[0-9]\+" 2 } } */
/* { dg-final { scan-assembler-times "\tvcvtps2ph\t\\\$4, %ymm\[0-9]\+, %xmm\[0-9]\+" 2 } } */
/* { dg-final { scan-assembler-not "\tvcvtps2ph\t\\\$4, %zmm\[0-9]\+, %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t%xmm\[0-9]\+, %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t%xmm\[0-9]\+, %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler-not "\tvcvtph2ps\t%ymm\[0-9]\+, %zmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t\\\(%\[re]\[a-z0-9]\+\\\), %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t\\\(%\[re]\[a-z0-9]\+\\\), %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler-not "\tvcvtph2ps\t\\\(%\[re]\[a-z0-9]\+\\\), %zmm\[0-9]\+" } } */

#include "pr121688-f16c.c"
