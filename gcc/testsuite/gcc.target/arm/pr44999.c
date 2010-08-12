/* Use UXTB to extract the lowest byte.  */
/* { dg-options "-mthumb -Os" } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { scan-assembler "uxtb" } } */

int tp(int x, int y)
{
  return (x & 0xff) - (y & 0xffff);
}
