/* { dg-options "-msoft-float" } */

void
foo (void)
{
  register float x asm ("$f0"); /* { dg-error "cannot be accessed" } */
}
