/* { dg-do compile } */

void
foo (void)
{
  register int cc __asm ("cc"); /* { dg-error "invalid register name" } */
  __asm ("" : : "r" (cc) : "cc");
}
