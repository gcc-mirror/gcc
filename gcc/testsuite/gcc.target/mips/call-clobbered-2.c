/* Check that we handle call-clobbered FPRs correctly.  */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-options "-mabi=32 -modd-spreg -mfp32 -ffixed-f0 -ffixed-f1 -ffixed-f2 -ffixed-f3 -ffixed-f4 -ffixed-f5 -ffixed-f6 -ffixed-f7 -ffixed-f8 -ffixed-f9 -ffixed-f10 -ffixed-f11 -ffixed-f12 -ffixed-f13 -ffixed-f14 -ffixed-f15 -ffixed-f16 -ffixed-f17 -ffixed-f18 -ffixed-f19 -ffixed-f20 -ffixed-f22 -ffixed-f24 -ffixed-f26 -ffixed-f28 -ffixed-f30" } */

void bar (void);
float a;
float
foo ()
{
  float b = a + 1.0f;
  bar();
  return b;
}
/* { dg-final { scan-assembler-times "lwc1" 4 } } */
/* { dg-final { scan-assembler-times "swc1" 2 } } */
/* { dg-final { scan-assembler-not "mtc" } } */
/* { dg-final { scan-assembler-not "mfc" } } */
/* { dg-final { scan-assembler-not "mthc" } } */
/* { dg-final { scan-assembler-not "mfhc" } } */
/* { dg-final { scan-assembler-not "sdc1" } } */
/* { dg-final { scan-assembler-not "ldc1" } } */
