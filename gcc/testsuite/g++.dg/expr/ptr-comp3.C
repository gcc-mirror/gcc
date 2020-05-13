// DR 1512
// PR c++/87699
// { dg-do compile { target c++11 } }
// { dg-options "-Wall -Wextra -pedantic-errors" }

/* Comparisons between pointer types with different cv-quals are now OK.  */

void
f (int **p1, const int **p2)
{
   if (p1 == p2) { }
   if (p1 != p2) { }
   if (p2 == p1) { }
   if (p2 != p1) { }
}
