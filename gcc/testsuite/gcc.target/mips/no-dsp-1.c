/* { dg-options "-mno-dsp -ffat-lto-objects" } */

void
foo (void)
{
  register int x asm ("$ac1hi"); /* { dg-error "cannot be accessed" } */
  asm volatile ("" : "=r" (x));
}
