/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-pg -mfentry -mnop-mcount" } */

void
profileme (void)
{
  /* { dg-final { scan-assembler "NOPs for -mnop-mcount \\(3 halfwords\\)\n.*brcl\t0,0" } } */
}
