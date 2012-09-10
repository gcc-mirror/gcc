/* { dg-options "-msoft-float -ffat-lto-objects" } */

void
foo (void)
{
  register float x asm ("$f0"); /* { dg-error "cannot be accessed" } */
  asm volatile ("" : "=r" (x));
}
