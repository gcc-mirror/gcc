/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only" } */

__attribute__((interrupt, naked))
void
fn (void *frame) /* { dg-error "not compatible" } */
{
}
