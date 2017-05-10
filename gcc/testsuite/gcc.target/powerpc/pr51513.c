/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-options "-O2 -fjump-tables --param case-values-threshold=1" } */
/* Verify we created a jump table.  */
/* { dg-final { scan-assembler-times "mtctr "  1 } } */
/* { dg-final { scan-assembler-times "bctr" 1 } } */
/* Verify we eliminated the range check.  */
/* { dg-final { scan-assembler-not "cmpldi" } } */
/* { dg-final { scan-assembler-not "cmplwi" } } */

long
bug (long cond, long v0, long v1, long v2)
{
  switch (cond)
    {
      case 0:
	return v0;
      case 1:
	return v1;
      case 2:
	return v2;
      default:
	__builtin_unreachable ();
    }
  __builtin_abort ();
}
