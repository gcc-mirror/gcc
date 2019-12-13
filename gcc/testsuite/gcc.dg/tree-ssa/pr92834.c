/* PR tree-optimization/92834 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR <" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR <" 8 "optimized" } } */

static inline unsigned
umax1 (unsigned a, unsigned b)
{
  return a - ((a - b) & -(a < b));
}

static inline unsigned
umin1 (unsigned a, unsigned b)
{
  return a - ((a - b) & -(a > b));
}

static inline int
smax1 (int a, int b)
{
  return a - ((a - b) & -(a < b));
}

static inline int
smin1 (int a, int b)
{
  return a - ((a - b) & -(a > b));
}

static inline unsigned long long
umax2 (unsigned long long a, unsigned long long b)
{
  return a - ((a - b) & -(a <= b));
}

static inline unsigned long long
umin2 (unsigned long long a, unsigned long long b)
{
  return a - ((a - b) & -(a >= b));
}

static inline long long
smax2 (long long a, long long b)
{
  return a - ((a - b) & -(a <= b));
}

static inline long long
smin2 (long long a, long long b)
{
  return a - ((a - b) & -(a >= b));
}

static inline unsigned
umax3 (unsigned a, unsigned b)
{
  return a + ((b - a) & -(a < b));
}

static inline unsigned
umin3 (unsigned a, unsigned b)
{
  return a + ((b - a) & -(a > b));
}

static inline int
smax3 (int a, int b)
{
  return a + ((b - a) & -(a < b));
}

static inline int
smin3 (int a, int b)
{
  return a + ((b - a) & -(a > b));
}

static inline unsigned long long
umax4 (unsigned long long a, unsigned long long b)
{
  return a + ((b - a) & -(a <= b));
}

static inline unsigned long long
umin4 (unsigned long long a, unsigned long long b)
{
  return a + ((b - a) & -(a >= b));
}

static inline long long
smax4 (long long a, long long b)
{
  return a + ((b - a) & -(a <= b));
}

static inline long long
smin4 (long long a, long long b)
{
  return a + ((b - a) & -(a >= b));
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
  x[8] = umax3 (x[6], x[7]);
  x[11] = umin3 (x[9], x[10]);
  y[8] = smax3 (y[6], y[7]);
  y[11] = smin3 (y[9], y[10]);
  z[8] = umax4 (z[6], z[7]);
  z[11] = umin4 (z[9], z[10]);
  w[8] = smax4 (w[6], w[7]);
  w[11] = smin4 (w[9], w[10]);
}
