/* Some versions of Solaris <math.h> give strict-aliasing warnings for
   signbit.  */
/* { dg-options "-std=c99 -O2 -Wstrict-aliasing" } */

#include <math.h>

int
main (void)
{
  return signbit (1.0f) | signbit (1.0) | signbit (1.0l);;
}
