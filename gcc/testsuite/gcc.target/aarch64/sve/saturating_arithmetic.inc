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
#define UT uint32_t
#define UMAX UINT_MAX
#define UMIN 0
#endif

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

void uaddq_imm (UT *out, UT *a, int n)
{
  for (int i = 0; i < n; i++)
    {
      UT sum = a[i] + 50;
      out[i] = sum < a[i] ? UMAX : sum;
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

void usubq_imm (UT *out, UT *a, int n)
{
  for (int i = 0; i < n; i++)
    {
      UT sum = a[i] - 50;
      out[i] = sum > a[i] ? UMIN : sum;
    }
}

#endif