/* { dg-options "-mthumb -O2" }  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */

int
foo (int v)
{
  register int i asm ("r0");
  register int j asm ("r1");
  if (v > 1)
    i = abs (j);

  return i;
}

