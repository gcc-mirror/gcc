/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void foo (void); /* { dg-note "previous declaration" } */

__attribute__ ((preserve_none))
void
foo (void) /* { dg-error "conflicting types" } */
{
}

