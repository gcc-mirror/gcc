// PR middle-end/121105
// { dg-do compile }
// { dg-options "-O2" }

#include <arm_sve.h>

svfloat32x4_t
e ()
{
  return svfloat32x4_t{};
}

// { dg-final { scan-assembler-times {\tmovi\td[0-3], #0\n} 4 } }
