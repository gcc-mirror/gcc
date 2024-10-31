#include <arm_sve.h>

#pragma GCC target "+nosme2+sve2+faminmax+sme"

svfloat32_t
foo (svfloat32_t x, svfloat32_t y) __arm_streaming
{
  return svamin_x (svptrue_b8 (), x, y); /* { dg-error {ACLE function '[^']*' requires ISA extension 'sme2'} } */
}
