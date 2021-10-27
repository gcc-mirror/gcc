/* Test for scanf formats.  Formats using C2X features.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic -Wformat" } */

#include "format.h"

void
foo (unsigned int *uip, unsigned short int *uhp, unsigned char *uhhp,
     unsigned long int *ulp, unsigned long long *ullp, uintmax_t *ujp,
     size_t *zp, unsigned_ptrdiff_t *utp)
{
  scanf ("%*b");
  scanf ("%2b", uip);
  scanf ("%hb%hhb%lb%llb%jb%zb%tb", uhp, uhhp, ulp, ullp, ujp, zp, utp);
  scanf ("%Lb", ullp); /* { dg-warning "does not support" } */
  scanf ("%qb", ullp); /* { dg-warning "does not support" } */
}
