/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mno-mmx" } */

void
 __attribute__ ((no_caller_saved_registers))
fn (void)
{ /* { dg-message "sorry, unimplemented: 80387 instructions aren't allowed" } */
}
