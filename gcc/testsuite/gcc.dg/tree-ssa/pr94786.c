/* PR tree-optimization/94786 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR <" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR <" 4 "optimized" } } */

static inline unsigned
umax1 (unsigned a, unsigned b)
{
  return a ^ ((a ^ b) & -(a < b));
}

static inline unsigned
umin1 (unsigned a, unsigned b)
{
  return a ^ ((a ^ b) & -(a > b));
}

static inline int
smax1 (int a, int b)
{
  return a ^ ((a ^ b) & -(a < b));
}

static inline int
smin1 (int a, int b)
{
  return a ^ ((a ^ b) & -(a > b));
}

static inline unsigned long long
umax2 (unsigned long long a, unsigned long long b)
{
  return a ^ ((a ^ b) & -(a <= b));
}

static inline unsigned long long
umin2 (unsigned long long a, unsigned long long b)
{
  return a ^ ((a ^ b) & -(a >= b));
}

static inline long long
smax2 (long long a, long long b)
{
  return a ^ ((a ^ b) & -(a <= b));
}

static inline long long
smin2 (long long a, long long b)
{
  return a ^ ((a ^ b) & -(a >= b));
}

void
test (unsigned *x, int *y, unsigned long long *z, long long *w)
{
  x[2] = umax1 (x[0], x[1]);
  x[5] = umin1 (x[2], x[3]);
  y[2] = smax1 (y[0], y[1]);
  y[5] = smin1 (y[2], y[3]);
  z[2] = umax2 (z[0], z[1]);
  z[5] = umin2 (z[2], z[3]);
  w[2] = smax2 (w[0], w[1]);
  w[5] = smin2 (w[2], w[3]);
}
