/* Test references to never-defined static functions in _Generic: still not
   allowed in a type name used for selection, only an expression (may change if
   "discarded" is adopted).  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

static int not_ok1 (); /* { dg-error "used but never defined" } */

void
f ()
{
  _Generic (int (*)[not_ok1 ()], default: 1);
}
