/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt2" } */

template <class T>
static inline const T&
min_ref (const T &x, const T &y)
{
  return x < y ? x : y;
}

int test_min_ref (int x, int y)
{
  return min_ref (x, y);
}

/* { dg-final { scan-tree-dump "MIN_EXPR" "phiopt2" } } */
