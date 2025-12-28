/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d --param=gpr2vr-cost=2 -fdump-tree-optimized" } */

#include <stdint.h>

#define T uint8_t

T
test_sat_add (T a, T b)
{
  return (a + b) | (-(T)((T)(a + b) < a));
}

void
test_sat_add_cost_1 (T * restrict out, T * restrict in,
		     T x, unsigned n)
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

/* { dg-final { scan-tree-dump ".SAT_ADD " "optimized" } } */
