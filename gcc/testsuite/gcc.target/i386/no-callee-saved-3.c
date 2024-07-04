/* { dg-do compile } */
/* { dg-options "-O2" } */

__attribute__ ((no_callee_saved_registers, no_caller_saved_registers))
void
foo (void) /* { dg-error "attributes are not compatible" } */
{
}
