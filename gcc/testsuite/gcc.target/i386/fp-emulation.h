#ifndef FP_EMULATION_H_INCLUDED
#define FP_EMULATION_H_INCLUDED

#include <math.h>

int is_snan(float x)
{
  union32f_ud fb;
  fb.f = x;
  return __builtin_isnan(x) && (fb.u & 0x00400000) == 0;
}

int is_qnan(float x)
{
  return __builtin_isnan(x) && !is_snan(x);
}

#define INTEL_SSE_MATH_OP(op, a, b) \
({ \
  union32f_ud tmp; \
  float result; \
  if (is_qnan(a)) \
    result = a; \
  else if (is_snan(a)) \
  { \
    tmp.f = a; tmp.u |= 0x400000; result = tmp.f; \
  } else if (is_snan(b)) \
  { \
    tmp.f = b; tmp.u |= 0x400000; result = tmp.f; \
  } else if (is_qnan(b)) \
    result = b; \
  else \
    result = a op b; \
  result; \
})

float
intel_sse_math_add(float a, float b)
{
  return INTEL_SSE_MATH_OP(+, a, b);
}

float
intel_sse_math_sub(float a, float b)
{
  return INTEL_SSE_MATH_OP(-, a, b);
}

float
intel_sse_math_mul(float a, float b)
{
  return INTEL_SSE_MATH_OP(*, a, b);
}

float
intel_sse_math_div(float a, float b)
{
  return INTEL_SSE_MATH_OP(/, a, b);
}

/* +-3 == +-inf,
   +-2 == +-Nan, use -2 only, 
   +-1 == +- normal number, 
     0 == undefined/not intialized */

int
state_handler(int src0, int src1, __int128 a, __int128 b, char op)
{
  /* Nan */
  if (src0 == -2 || src1 == -2)
    return -2;

  if (abs(src0) == 3 || abs(src1) == 3) 
    {
      /* +INF + +INF = +INF, -INF + -INF = -INF */
      if (src0 == src1) 
	return src0;
	
      /* Positive result */
      if ((op == '*' && ((src0 < 0 && b < 0) || (src1 < 0 && a < 0))) ||
          (op == '*' && ((src0 > 0 && b > 0) || (src1 > 0 && a > 0))))
	return 3;

      /* -INF * (positive normal) = -INF */
      if (op == '*' && ((src0 < 0 || src1 < 0) && (a >= 0 && b >= 0)))
	return -3;

      /* INF * 0 = NaN */
      if (a == 0 || b == 0)
	return -2;

      /* -INF + +INF = NaN, else +INF or -INF */
      return (op == '+' && src0 != src1) ? -2 : 3 * ((src0 / 3) + (src1 / 3));
    }

    /* Normal number case */
  __int128 result = op == '+' ? a + b : a * b;
  return result < 0 ? -1 : 1;
}

int
state_handler_float (float src)
{
  if (isnan (src))
      return -2;
  else if (isinf (src))
    return src > 0 ? 3 : -3;
  else
    return src >= 0 ? 1 : -1;
}

#endif
