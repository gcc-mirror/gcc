/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_fp16_ok } */
/* { dg-options "-mfp16-format=ieee" } */
/* { dg-add-options arm_neon_fp16 } */

/* Test generation of VFP __fp16 instructions.  */

__fp16 h1 = 0.0;
__fp16 h2 = 1234.0;
float f1 = 2.0;
float f2 = -999.9;

void f (void)
{
  h1 = f1;
  f2 = h2;
}

/* { dg-final { scan-assembler "\tvcvtb.f32.f16" } } */
/* { dg-final { scan-assembler "\tvcvtb.f16.f32" } } */
