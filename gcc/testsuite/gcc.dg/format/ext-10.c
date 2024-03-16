/* Test for scanf format extensions using formats from C23.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23 -Wformat" } */

#include "format.h"

void
foo (u_quad_t *uqp, unsigned long long int *ullp)
{
  /* Deprecated length modifiers with %b.  */
  scanf ("%qb", uqp);
  scanf ("%Lb", ullp);
}
