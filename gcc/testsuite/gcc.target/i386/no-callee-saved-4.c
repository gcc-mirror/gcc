/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only" } */

__attribute__ ((no_callee_saved_registers, interrupt))
void
foo (void *frame) /* { dg-error "attributes are not compatible" } */
{
}
