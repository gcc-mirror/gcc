/* PR target/121688 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512fp16 -masm=att" } */
/* { dg-final { scan-assembler "\tvcvtps2phxx\t%xmm\[0-9]\+, %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtps2phxy\t%ymm\[0-9]\+, %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtps2phx\t%zmm\[0-9]\+, %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtps2phxx\t\\\(%\[re]\[a-z0-9]\+\\\), %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtps2phxy\t\\\(%\[re]\[a-z0-9]\+\\\), %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtps2phx\t\\\(%\[re]\[a-z0-9]\+\\\), %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2psx\t%xmm\[0-9]\+, %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2psx\t%xmm\[0-9]\+, %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2psx\t%ymm\[0-9]\+, %zmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2psx\t\\\(%\[re]\[a-z0-9]\+\\\), %xmm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2psx\t\\\(%\[re]\[a-z0-9]\+\\\), %ymm\[0-9]\+" } } */
/* { dg-final { scan-assembler "\tvcvtph2psx\t\\\(%\[re]\[a-z0-9]\+\\\), %zmm\[0-9]\+" } } */

#include "pr121688-f16c.c"
