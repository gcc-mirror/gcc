/* { dg-do compile } */
/* PR middle-end/116259 */

#include <arm_sve.h>

/* PAREN_EXPR lowering for VLA vectors was ICEing.
   It should not be lowered in a similar way as moves
   are not lowered.  */
svfloat64_t f(svfloat64_t x)
{
  return __builtin_assoc_barrier(x);
}
