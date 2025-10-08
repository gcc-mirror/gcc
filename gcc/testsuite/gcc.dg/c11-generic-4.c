/* Test references to never-defined static functions in _Generic: allowed in
   certain places for C23 but not before.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

static int ok1_c23 (); /* { dg-error "used but never defined" } */
static int ok2_c23 (); /* { dg-error "used but never defined" } */
static int ok3_c23 (); /* { dg-error "used but never defined" } */
static int ok4_c23 (); /* { dg-error "used but never defined" } */
static int ok5_c23 (); /* { dg-error "used but never defined" } */
static int ok6 ();
static int ok7 ();
static int ok8 ();
static int ok9 ();
static int ok10 ();
static int ok11 ();
static int ok12 ();
static int not_ok1 (); /* { dg-error "used but never defined" } */
static int not_ok2 (); /* { dg-error "used but never defined" } */

void
f ()
{
  _Generic (ok1_c23 (), int: 2);
  _Generic (1, int: 2, default: ok2_c23 ());
  _Generic (1, default: ok3_c23 (), int: 3);
  _Generic (1, int: 2, float: ok4_c23 ());
  _Generic (1, float: ok5_c23 (), int: 3);
  sizeof (_Generic (ok8 (), int: 2));
  sizeof (_Generic (1, int: 2, default: ok9 ()));
  sizeof (_Generic (1, default: ok10 (), int: 3));
  sizeof (_Generic (1, int: 2, float: ok11 ()));
  sizeof (_Generic (1, float: ok12 (), int: 3));
  _Generic (1.0, int: 2, default: not_ok1 ());
  _Generic (1.0, default: not_ok2 (), int: 3);
  sizeof (_Generic (1.0, int: 2, default: ok6 ()));
  sizeof (_Generic (1.0, default: ok7 (), int: 3));
}
