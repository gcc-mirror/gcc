/* { dg-options "-msoft-float -ffat-lto-objects" } */
/* This is testing for errors which can only happen in assembly generation.
   dg-error does not guarantee assembly generation, so we need to do it
   manually by using -ffat-lto-objects.  */

void
foo (void)
{
  register float x asm ("$f0"); /* { dg-error "cannot be accessed" } */
  asm volatile ("" : "=r" (x));
}
