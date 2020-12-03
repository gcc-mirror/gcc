/* { dg-options "-O2 -W -Wall -Werror" } */

#include <arm_sve.h>

svfloat32x2_t
foo (svfloat32_t x, svfloat32_t y)
{
  svfloat32x2_t res = svundef2_f32 ();
  res = svset2 (res, 0, x);
  res = svset2 (res, 1, y);
  return res;
}
