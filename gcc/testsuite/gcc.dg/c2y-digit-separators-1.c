/* Test C2Y digit separators.  Valid usages.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

_Static_assert (0o3'703 == 0O3703);
_Static_assert (0O1'7'7'7 == 0o1777);
