/* Check that we move DFmode values using mtc1 between FP and GP.  */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-options "-mabi=32 -mfp32 isa=2" } */

void bar (void);

double
foo (int x, double a)
{
  return a;
}
/* { dg-final { scan-assembler-times "mtc1" 2 } } */
/* { dg-final { scan-assembler-not "ldc1" } } */
