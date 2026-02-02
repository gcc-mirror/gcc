/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=0 --param=vr2gpr-cost=0 -fdump-tree-optimized" } */

#include <stdint.h>

#define T int32_t
#define UT uint32_t

T
test_sat_add (T x, T y)
{
  T sum = (UT)x + (UT)y;
  return (x ^ y) < 0
    ? sum
    : (sum ^ x) >= 0
      ? sum
      : x < 0 ? INT32_MIN : INT32_MAX;
}

void
test_vx_binary_sat_add (T * restrict out, T * restrict in, T x, unsigned n)
{
  unsigned k = 0;
  T tmp = x + 3;

  while (k < n)
    {
      tmp = tmp ^ 0x82;
      out[k + 0] = test_sat_add (in[k + 0], tmp);
      out[k + 1] = test_sat_add (in[k + 1], tmp);
      k += 2;

      out[k + 0] = test_sat_add (in[k + 0], tmp);
      out[k + 1] = test_sat_add (in[k + 1], tmp);
      k += 2;

      out[k + 0] = test_sat_add (in[k + 0], tmp);
      out[k + 1] = test_sat_add (in[k + 1], tmp);
      k += 2;

      out[k + 0] = test_sat_add (in[k + 0], tmp);
      out[k + 1] = test_sat_add (in[k + 1], tmp);
      k += 2;
    }
}

/* { dg-final { scan-assembler {vsadd.vx} } } */
/* { dg-final { scan-tree-dump ".SAT_ADD " "optimized" } } */
