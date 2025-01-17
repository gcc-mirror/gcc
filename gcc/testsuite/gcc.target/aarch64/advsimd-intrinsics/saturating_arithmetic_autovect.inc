/* Template file for vector saturating arithmetic validation.

   This file defines saturating addition and subtraction functions for a given
   scalar type, testing the auto-vectorization of these two operators. This
   type, along with the corresponding minimum and maximum values for that type,
   must be defined by any test file which includes this template file.  */

#ifndef SAT_ARIT_AUTOVEC_INC
#define SAT_ARIT_AUTOVEC_INC

#include <limits.h>
#include <arm_neon.h>

#ifndef UT
#define UT unsigned int
#define VT uint32x4_t
#define UMAX UINT_MAX
#define UMIN 0
#endif


UT uadd_lane (UT a, VT b)
{
  UT sum = a + b[0];
  return sum < a ? UMAX : sum;
}

void uaddq (UT *out, UT *a, UT *b, int n)
{
  for (int i = 0; i < n; i++)
    {
      UT sum = a[i] + b[i];
      out[i] = sum < a[i] ? UMAX : sum;
    }
}

void uaddq2 (UT *out, UT *a, UT *b, int n)
{
  for (int i = 0; i < n; i++)
    {
      UT sum;
      if (!__builtin_add_overflow(a[i], b[i], &sum))
        out[i] = sum;
      else
        out[i] = UMAX;
    }
}

void usubq (UT *out, UT *a, UT *b, int n)
{
  for (int i = 0; i < n; i++)
    {
      UT sum = a[i] - b[i];
      out[i] = sum > a[i] ? UMIN : sum;
    }
}

#endif