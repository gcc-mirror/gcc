/* { dg-do compile } */

#include <arm_sve.h>

void unprototyped ();

void
f (svuint8_t *ptr)
{
  unprototyped (*ptr); /* { dg-error {SVE type '(svuint8_t|__SVUint8_t)' cannot be passed to an unprototyped function} } */
}
