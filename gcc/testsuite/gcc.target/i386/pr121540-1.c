/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

void
 __attribute__ ((no_caller_saved_registers))
fn (void)
{ /* { dg-message "sorry, unimplemented: MMX/3Dnow instructions aren't allowed" } */
}
