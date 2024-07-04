/* Test C2Y _Generic features: error with -std=c23 -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

_Static_assert (_Generic (const int, int : 1, const int : 2) == 2); /* { dg-error "use of type name" } */

_Static_assert (_Generic (void, int : 1, void : 2) == 2); /* { dg-error "use of type name" } */
/* { dg-error "incomplete type" "incomplete type" { target *-*-* } .-1 } */

_Static_assert (_Generic (int (), int (*) () : 1, int () : 2) == 2); /* { dg-error "use of type name" } */
/* { dg-error "function type" "function type" { target *-*-* } .-1 } */

const int ci;

_Static_assert (_Generic (typeof (ci), const int : 1, int : 2) == 1); /* { dg-error "use of type name" } */

_Static_assert (_Generic (ci, const int : 1, int : 2) == 2);
