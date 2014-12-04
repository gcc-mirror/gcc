/* Check that we handle call-clobbered FPRs correctly.  */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-options "isa>=2 -mabi=32 -mhard-float -ffixed-f0 -ffixed-f1 -ffixed-f2 -ffixed-f3 -ffixed-f4 -ffixed-f5 -ffixed-f6 -ffixed-f7 -ffixed-f8 -ffixed-f9 -ffixed-f10 -ffixed-f11 -ffixed-f12 -ffixed-f13 -ffixed-f14 -ffixed-f15 -ffixed-f16 -ffixed-f17 -ffixed-f18 -ffixed-f19" } */

void bar (void);
double a;
double
foo ()
{
  double b = a + 1.0;
  bar();
  return b;
}
/* { dg-final { scan-assembler-not "lwc1" } } */
/* { dg-final { scan-assembler-not "swc1" } } */
/* { dg-final { scan-assembler-times "sdc1" 2 } } */
/* { dg-final { scan-assembler-times "ldc1" 4 } } */
/* { dg-final { scan-assembler-not "mtc" } } */
/* { dg-final { scan-assembler-not "mfc" } } */
/* { dg-final { scan-assembler-not "mthc" } } */
/* { dg-final { scan-assembler-not "mfhc" } } */
