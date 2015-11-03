/* Check that displacement addressing is used for indexed addresses with a
   small offset, instead of re-calculating the index and that the movu.w
   instruction is used on SH2A.  */
/* { dg-do compile { target { sh2a } } }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "add\t#1" } } */
/* { dg-final { scan-assembler "movu.w" } } */

int
test_00 (unsigned short tab[], int index)
{
  return tab[index + 1];
}
