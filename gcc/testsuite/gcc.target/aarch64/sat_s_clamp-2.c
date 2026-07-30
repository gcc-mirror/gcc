/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

#include <limits.h>

static inline int imin (int a, int b) { return a < b ? a : b; }
static inline int imax (int a, int b) { return a > b ? a : b; }

void
add_loop (short *__restrict d, short *__restrict a, short *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    {
      int t = a[i] + b[i];
      d[i] = (short) imax (imin (t, 32767), -32768);
    }
}

void
sat_loop (int *__restrict d, int *__restrict a, int *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    {
      long long t = (long long) a[i] + b[i];
      if (t > INT_MAX) t = INT_MAX;
      if (t < INT_MIN) t = INT_MIN;
      d[i] = (int) t;
    }
}

/* The clamp of an equal precision sum is not a saturating add: the
   comparisons are dead and the whole thing is a plain addition.  */

int
not_saturating (int x, int y)
{
  int t = x + y;
  return t > INT_MAX ? INT_MAX : t < INT_MIN ? INT_MIN : t;
}

/* { dg-final { scan-tree-dump "\\.SAT_ADD " "optimized" } } */
/* { dg-final { scan-assembler "sqadd\tv\[0-9\]+\.8h" } } */
/* { dg-final { scan-assembler "sqadd\tv\[0-9\]+\.4s" } } */
