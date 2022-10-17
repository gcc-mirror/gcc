/* Test for printf format extensions using formats from or recommended by
   C2X.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2x -Wformat" } */

#include "format.h"

void
foo (u_quad_t uq, unsigned int u, unsigned short us, unsigned char uc,
     unsigned long ul, unsigned long long ull, uintmax_t uj, size_t z,
     unsigned_ptrdiff_t ut)
{
  /* Deprecated length modifiers with %b and %B.  */
  printf ("%qb%qB", uq, uq);
  printf ("%Lb%LB", ull, ull);
  printf ("%Zb%ZB", z, z);
  /* Use of %B in cases valid for %b.  */
  printf ("%B %hB %hhB %lB %llB %jB %zB %tB\n", u, us, uc, ul, ull, uj, z, ut);
  printf ("%*.*llB\n", 1, 2, ull);
  printf ("%-B\n", u);
  printf ("%#B\n", u);
  printf ("%08B\n", u);
  /* Flags valid on signed conversions only.  */
  printf ("%+B\n", u); /* { dg-warning "flag" } */
  printf ("% B\n", u); /* { dg-warning "flag" } */
  /* Flags ignored in certain combinations.  */
  printf ("%-08B\n", u); /* { dg-warning "ignored" } */
  printf ("%08.5B\n", u); /* { dg-warning "ignored" } */
}
