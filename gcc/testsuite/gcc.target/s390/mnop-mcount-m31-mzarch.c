/* { dg-do compile } */
/* { dg-options "-m31 -mzarch -pg -mnop-mcount" } */

void
profileme (void)
{
  /* { dg-final { scan-assembler "NOPs for -mnop-mcount \\(10 halfwords\\)\n.*brcl\t0,0\n.*brcl\t0,0\n.*brcl\t0,0\n.*bcr\t0,0" } } */
}
