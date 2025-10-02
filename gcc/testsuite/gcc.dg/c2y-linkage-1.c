/* Test mixing internal and external linker for the same identifier (a
   constraint violation in C2y).  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

static int x;

void
f ()
{
  long x;
  {
    extern int x; /* { dg-error "variable previously declared 'static' redeclared 'extern'" } */
  }
}
