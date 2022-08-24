/* Skip this on aix, since it takes soft-float and long-double-128
   incompatible and warns it.  */
/* { dg-skip-if "aix long-double-128 soft-float" { powerpc*-*-aix* } } */
/* { dg-options "-mlong-double-128 -msoft-float" } */

/* Verify there is no ICE.  */

#include <stddef.h>
#include <stdlib.h>
#include <math.h>

#define PACK __builtin_pack_ibm128
#define UNPACK __builtin_unpack_ibm128
#define LDOUBLE __ibm128

extern LDOUBLE bar (LDOUBLE);

int
main (void)
{
  double high = pow (2.0, 60);
  double low = 2.0;
  LDOUBLE a = ((LDOUBLE) high) + ((LDOUBLE) low);
  double x0 = UNPACK (a, 0);
  double x1 = UNPACK (a, 1);
  LDOUBLE b = PACK (x0, x1);
  LDOUBLE c = bar (b);

  return c > a;
}

