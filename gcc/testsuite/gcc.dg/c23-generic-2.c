/* Test C2Y _Generic features: warning with -std=c23 -pedantic.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic" } */

_Static_assert (_Generic (const int, int : 1, const int : 2) == 2); /* { dg-warning "use of type name" } */

_Static_assert (_Generic (void, int : 1, void : 2) == 2); /* { dg-warning "use of type name" } */
/* { dg-warning "incomplete type" "incomplete type" { target *-*-* } .-1 } */

_Static_assert (_Generic (int (), int (*) () : 1, int () : 2) == 2); /* { dg-warning "use of type name" } */
/* { dg-warning "function type" "function type" { target *-*-* } .-1 } */

const int ci;

_Static_assert (_Generic (typeof (ci), const int : 1, int : 2) == 1); /* { dg-warning "use of type name" } */

_Static_assert (_Generic (ci, const int : 1, int : 2) == 2);
