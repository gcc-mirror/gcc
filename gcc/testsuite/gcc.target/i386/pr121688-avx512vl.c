/* PR target/121688 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-f16c -mavx512vl -mno-avx512fp16 -masm=att" } */
/* { dg-final { scan-assembler-times "\tvcvtps2ph\t\\\$4, %xmm\[0-9]\+, %xmm\[0-9]\+" 2 } } */
/* { dg-final { scan-assembler-times "\tvcvtps2ph\t\\\$4, %ymm\[0-9]\+, %xmm\[0-9]\+" 2 } } */
/* { dg-final { scan-assembler-times "\tvcvtps2ph\t\\\$4, %zmm\[0-9]\+, %ymm\[0-9]\+" 2 } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t%xmm\[0-9]\+, %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t%xmm\[0-9]\+, %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t%ymm\[0-9]\+, %zmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t\\\(%\[re]\[a-z0-9]\+\\\), %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t\\\(%\[re]\[a-z0-9]\+\\\), %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2ps\t\\\(%\[re]\[a-z0-9]\+\\\), %zmm\[0-9]\+" } } */

#include "pr121688-f16c.c"
