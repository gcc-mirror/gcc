/* { dg-do compile } */
/* { dg-options "-march=mips1 -fno-delayed-branch" } */
/* { dg-final { scan-assembler "\tbne\t.*\tnop" } } */

/* Ensure that mips1 does not put anything in the delay slot of the bne
   instruction when checking for divide by zero.  mips2+ systems use teq
   instead of bne and teq has no delay slot.  */

NOCOMPRESSION int
foo (int a, int b)
{
  return a / b;
}
