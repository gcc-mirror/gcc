/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-final { scan-assembler {\.LC0:\n\t\.long\t1078523331\n} } } */


/*
** test_float_into_gpr:
**     lrl	%r4,.LC0
**     foo	%r4
**     br	%r14
*/

void
test_float_into_gpr (void)
{
  // This is the counterpart to
  //   register float x asm ("r4") = 3.14f;
  //   asm ("foo	%0" :: "r" (x));
  // where the bit-pattern of 3.14f is loaded into GPR.
  asm ("foo	%0" :: "{r4}" (3.14f));
}

/*
** test_float:
** (
**     ldr	%f4,%f0
**     ldr	%f5,%f2
** |
**     ldr	%f5,%f2
**     ldr	%f4,%f0
** )
**     aebr	%f5,%f4
**     ldr	%f0,%f5
**     br	%r14
*/

float
test_float (float x, float y)
{
  asm ("aebr	%0,%1" : "+{f5}" (y) : "{f4}" (x));
  return y;
}
