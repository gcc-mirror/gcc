/* Test ICE for statement expression ending with asm as asm input (bug
   100792).  */
/* { dg-do compile } */
/* { dg-options "" } */

void
f ()
{
  __asm__ ("" : : "m" (({ __asm__ (""); }))); /* { dg-error "memory input 0 is not directly addressable" } */
}
