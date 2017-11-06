/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=hard -mfpu=fpv5-sp-d16" }  */
/* { dg-require-effective-target arm_arch_v8m_main_ok } */
/* { dg-add-options arm_arch_v8m_main } */
/* { dg-skip-if "Do not combine float-abi= hard | soft | softfp" {*-*-*} {"-mfloat-abi=soft" -mfloat-abi=softfp } {""} } */
/* { dg-skip-if "Skip these if testing double precision" {*-*-*} {"-mfpu=fpv[4-5]-d16"} {""} } */

int __attribute__ ((cmse_nonsecure_call)) (*bar) (double);

int
foo (int a)
{
  return bar (2.0) + a + 1;
}

/* Checks for saving and clearing prior to function call.  */
/* { dg-final { scan-assembler "lsrs\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "lsls\tr4, r4, #1" } } */
/* { dg-final { scan-assembler "mov\tr0, r4" } } */
/* { dg-final { scan-assembler "mov\tr1, r4" } } */
/* { dg-final { scan-assembler "mov\tr2, r4" } } */
/* { dg-final { scan-assembler "mov\tr3, r4" } } */
/* { dg-final { scan-assembler-not "vldr\.32\ts0, .L" } } */
/* { dg-final { scan-assembler-not "vldr\.32\ts1, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts2, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts3, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts4, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts5, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts6, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts7, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts8, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts9, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts10, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts11, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts12, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts13, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts14, .L" } } */
/* { dg-final { scan-assembler "vldr\.32\ts15, .L" } } */

/* Now we check that we use the correct intrinsic to call.  */
/* { dg-final { scan-assembler "bl\t__gnu_cmse_nonsecure_call" } } */
