/* Test mixing internal and external linker for the same identifier (a
   constraint violation in C2y).  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

void
f ()
{
  extern int x (); /* { dg-message "previous declaration" } */
}

static int x (); /* { dg-error "static declaration of 'x' follows non-static declaration" } */
