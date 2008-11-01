/* Make sure ADDI is combined and emitted successfully.
   See also PR37939.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "addi" } } */
/* { dg-final { scan-assembler-not "lsl" } } */

int xyzzy (int r10, int r11)
{
  return r11 * 4 + r10;
}
