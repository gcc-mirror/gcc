/* PR tree-optimization/109906 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */
/* { dg-require-effective-target int32 } */

/* Implementation of rotate right operation */
static inline
unsigned rrotate(unsigned x, int t)
{
  if (t >= 32) __builtin_unreachable();
  unsigned tl = x >> (t);
  unsigned th = x << (32 - t);
  return tl | th;
}

/* Here rotate left is achieved by doing rotate right by (32 - x) */
unsigned rotateleft(unsigned t, int x)
{
  return rrotate (t, 32 - x);
}

/* Implementation of rotate left operation */
static inline
unsigned lrotate(unsigned x, int t)
{
  if (t >= 32) __builtin_unreachable();
  unsigned tl = x << (t);
  unsigned th = x >> (32 - t);
  return tl | th;
}

/* Here rotate right is achieved by doing rotate left by (32 - x) */
unsigned rotateright(unsigned t, int x)
{
  return lrotate (t, 32 - x);
}

/* Shouldn't have instruction for (32 - x). */
/* { dg-final { scan-tree-dump-not "minus_expr" "optimized" } } */
/* { dg-final { scan-tree-dump "rrotate_expr" "optimized" } } */
/* { dg-final { scan-tree-dump "lrotate_expr" "optimized" } } */
