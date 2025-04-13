/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only" } */

__attribute__ ((preserve_none, interrupt))
void
foo (void *frame) /* { dg-error "attributes are not compatible" } */
{
}
