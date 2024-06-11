/* Test C2Y _Generic features.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

_Static_assert (_Generic (const int, int : 1, const int : 2) == 2);

_Static_assert (_Generic (void, int : 1, void : 2) == 2);

_Static_assert (_Generic (int (), int (*) () : 1, int () : 2) == 2);

const int ci;

_Static_assert (_Generic (typeof (ci), const int : 1, int : 2) == 1);

_Static_assert (_Generic (ci, const int : 1, int : 2) == 2);
