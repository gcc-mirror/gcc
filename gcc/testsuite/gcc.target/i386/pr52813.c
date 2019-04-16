/* Ensure that stack pointer cannot be an asm clobber.  */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

void
test1 (void)
{
  asm volatile ("" : : : "%esp"); /* { dg-warning "listing the stack pointer register '%esp' in a clobber list is deprecated" } */
  /* { dg-message "note: the value of the stack pointer after an 'asm' statement must be the same as it was before the statement" "" { target *-*-* } .-1 } */
}
