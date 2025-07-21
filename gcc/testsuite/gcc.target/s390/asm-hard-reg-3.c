/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-final { scan-assembler {\.LC0:\n\t\.long\t1074339512\n\t\.long\t1374389535\n} } } */

/*
** test_double_into_gpr:
**     lgrl	%r4,.LC0
**     foo	%r4
**     br	%r14
*/

void
test_double_into_gpr (void)
{
  // This is the counterpart to
  //   register double x asm ("r4") = 3.14;
  //   asm ("foo	%0" :: "r" (x));
  // where the bit-pattern of 3.14 is loaded into GPR.
  asm ("foo	%0" :: "{r4}" (3.14));
}

/*
** test_double:
** (
**     ldr	%f4,%f0
**     ldr	%f5,%f2
** |
**     ldr	%f5,%f2
**     ldr	%f4,%f0
** )
**     adbr	%f5,%f4
**     ldr	%f0,%f5
**     br	%r14
*/

double
test_double (double x, double y)
{
  asm ("adbr	%0,%1" : "+{f5}" (y) : "{f4}" (x));
  return y;
}
