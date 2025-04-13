/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void foo (void) __attribute__ ((preserve_none)); /* { dg-note "previous declaration" } */

void
foo (void) /* { dg-error "conflicting types" } */
{
}

