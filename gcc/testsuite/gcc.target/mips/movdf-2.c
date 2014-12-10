/* Check that we move DFmode values using mthc between FP and GP.  */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-options "-mabi=32 -mfpxx isa_rev=2" } */

void bar (void);

double
foo (int x, double a)
{
  return a;
}
/* { dg-final { scan-assembler "mthc1" } } */
/* { dg-final { scan-assembler "mtc1" } } */
/* { dg-final { scan-assembler-not "ldc1" } } */
