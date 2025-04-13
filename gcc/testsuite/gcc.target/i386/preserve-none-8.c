/* { dg-do compile } */
/* { dg-options "-O2" } */

__attribute__ ((preserve_none, no_caller_saved_registers))
void
foo (void)
{ /* { dg-error "attributes are not compatible" } */
}
