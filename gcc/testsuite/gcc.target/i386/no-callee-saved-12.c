/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void foo (void) __attribute__ ((no_callee_saved_registers)); /* { dg-note "previous declaration" } */

void
foo (void) /* { dg-error "conflicting types" } */
{
}

