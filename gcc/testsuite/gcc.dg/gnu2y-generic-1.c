/* Test C2Y _Generic features: __extension__ suppresses -Wc23-c2y-compat
   warnings (and the state is restored after __extension__).  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2y -pedantic-errors -Wc23-c2y-compat" } */

_Static_assert (__extension__ _Generic (const int, int : 1, const int : 2) == 2);
_Static_assert (_Generic (const int, int : 1, const int : 2) == 2); /* { dg-warning "use of type name" } */

_Static_assert (__extension__ _Generic (void, int : 1, void : 2) == 2);
_Static_assert (_Generic (void, int : 1, void : 2) == 2); /* { dg-warning "use of type name" } */
/* { dg-warning "incomplete type" "incomplete type" { target *-*-* } .-1 } */

_Static_assert (__extension__ _Generic (int (), int (*) () : 1, int () : 2) == 2);
_Static_assert (_Generic (int (), int (*) () : 1, int () : 2) == 2); /* { dg-warning "use of type name" } */
/* { dg-warning "function type" "function type" { target *-*-* } .-1 } */
