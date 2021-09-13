/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-pg -mnop-mcount" } */

void
profileme (void)
{
  /* { dg-final { scan-assembler "NOPs for -mnop-mcount \\(9 halfwords\\)\n.*brcl\t0,0\n.*brcl\t0,0\n.*brcl\t0,0" } } */
}
