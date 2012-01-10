/* { dg-options "-mno-dsp" } */

void
foo (void)
{
  register int x asm ("$ac1hi"); /* { dg-error "cannot be accessed" } */
}
