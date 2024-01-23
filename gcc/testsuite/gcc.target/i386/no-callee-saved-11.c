/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void foo (void); /* { dg-note "previous declaration" } */

__attribute__ ((no_callee_saved_registers))
void
foo (void) /* { dg-error "conflicting types" } */
{
}

