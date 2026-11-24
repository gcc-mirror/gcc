/* PR tree-optimization/128904 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

/* The vclzq_u32 call is folded to a one-argument vector .CLZ, which if
   conversion must leave alone when it makes the call unconditional.  */
uint32x4_t
f (uint32x4_t x, uint32x4_t *p, int n)
{
  uint32x4_t s = vdupq_n_u32 (0);
  for (int i = 0; i < n; i++)
    if (p[i][0] != 5)
      s = vaddq_u32 (s, vclzq_u32 (x));
  return s;
}
