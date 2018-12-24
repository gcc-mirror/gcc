/* Ensure that stack pointer cannot be an asm clobber.  */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

void
test1 (void)
{
  asm volatile ("" : : : "%esp"); /* { dg-error "Stack Pointer register clobbered" } */
}
