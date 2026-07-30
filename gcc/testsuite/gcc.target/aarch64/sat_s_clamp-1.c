/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include <limits.h>

static inline int imin (int a, int b) { return a < b ? a : b; }
static inline int imax (int a, int b) { return a > b ? a : b; }

/* A signed saturating add or subtract written as a two sided clamp on a
   wider intermediate.  Both nesting orders appear, and the narrow types
   reach this shape through the integer promotions.  */

int
add_ll (int x, int y)
{
  long long t = (long long) x + y;
  if (t > INT_MAX) t = INT_MAX;
  if (t < INT_MIN) t = INT_MIN;
  return (int) t;
}

int
add_ll_else (int x, int y)
{
  long long t = (long long) x + y;
  if (t > INT_MAX) t = INT_MAX;
  else if (t < INT_MIN) t = INT_MIN;
  return (int) t;
}

int
sub_ll (int x, int y)
{
  long long t = (long long) x - y;
  if (t > INT_MAX) t = INT_MAX;
  if (t < INT_MIN) t = INT_MIN;
  return (int) t;
}

short
add_hi (short x, short y)
{
  int t = x + y;
  return (short) imax (imin (t, 32767), -32768);
}

short
sub_hi (short x, short y)
{
  int t = x - y;
  return (short) imin (imax (t, -32768), 32767);
}

signed char
add_qi (signed char x, signed char y)
{
  int t = x + y;
  return (signed char) imax (imin (t, 127), -128);
}

signed char
sub_qi (signed char x, signed char y)
{
  int t = x - y;
  return (signed char) imin (imax (t, -128), 127);
}

signed char
add_qi_shared (signed char x, signed char y, int *p)
{
  int t = x + y;
  int lo = imax (t, -128);
  *p = lo;
  return (signed char) imin (lo, 127);
}

/* { dg-final { scan-tree-dump-times "\\.SAT_ADD " 5 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\.SAT_SUB " 3 "optimized" } } */
