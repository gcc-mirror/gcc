/* Use 16-bit multiply instruction in Thumb-2 mode when optimizing for
   size.  */
/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { scan-assembler "muls" } } */

int f(int i, int j) 
{
  return i * j;
}
