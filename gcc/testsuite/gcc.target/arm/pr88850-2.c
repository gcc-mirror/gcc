/* PR target/88850.  */
/* { dg-do compile } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } {"-mfloat-abi=softfp" } } */
/* { dg-additional-options "-O2 -march=armv7-a -fdump-rtl-final" } */
/* { dg-add-options arm_neon_softfp_fp16 } */
/* { dg-require-effective-target arm_neon_softfp_fp16_ok } */

#include <arm_neon.h>

extern void c (int, float16x4_t);

void a (float16x4_t b)
{
  c (0, b);
}


/* Check that these 64-bit moves are done in SI.  */
/* { dg-final { scan-rtl-dump "_movsi_vfp" "final" } } */
