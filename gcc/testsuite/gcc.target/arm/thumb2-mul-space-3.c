/* In Thumb-2 mode, when optimizing for size, generate a "muls"
   instruction and use the resulting condition flags rather than a
   separate compare instruction.  */
/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { scan-assembler "muls" } } */
/* { dg-final { scan-assembler-not "cmp" } } */

int x;

int f(int i, int j)
{
  i = i * j;
  if (i < 0)
    x = 1;
  return i;
}
