/* Make sure ADDI is combined and emitted successfully.
   See also PR37939.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "\taddi" 2 } } */
/* { dg-final { scan-assembler-not "\tlsl|\tmul|\tmove|\tadd\[^i\]" } } */

int xyzzy (int r10, int r11)
{
  return r11 * 4 + r10;
}

int plugh (int r10, int r11)
{
  return r11 * 2 + r10;
}
