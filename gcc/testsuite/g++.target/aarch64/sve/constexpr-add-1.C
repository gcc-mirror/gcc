/* { dg-do compile } */

/* PR C++/118445 */

#include <arm_sve.h>

/* See if constexpr handles VEC_DUPLICATE and SVE. */
constexpr svfloat32_t f(svfloat32_t b, float a)
{
  return b + a;
}

svfloat32_t g(void)
{
  return f((svfloat32_t){1.0}, 2.0);
}
