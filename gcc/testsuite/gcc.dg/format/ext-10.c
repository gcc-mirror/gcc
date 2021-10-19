/* Test for scanf format extensions using formats from C2X.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2x -Wformat" } */

#include "format.h"

void
foo (u_quad_t *uqp, unsigned long long int *ullp)
{
  /* Deprecated length modifiers with %b.  */
  scanf ("%qb", uqp);
  scanf ("%Lb", ullp);
}
