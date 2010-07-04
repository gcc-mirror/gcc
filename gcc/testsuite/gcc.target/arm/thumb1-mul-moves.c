/* Check for unnecessary register moves.  */
/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb1_ok } */

int f(int x)
{
  return x*42;
}

/* { dg-final { scan-assembler-not "mov\[\\t \]*r0," } } */

