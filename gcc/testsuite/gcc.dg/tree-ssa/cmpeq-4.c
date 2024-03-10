/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-original" } */
/* PR tree-optimization/107881 */

_Bool ltgt_eq(int a, int b)
{
  _Bool c = a < b;
  _Bool d = a > b;
  return c == d; // a == b
}
/* { dg-final { scan-tree-dump "a_\[0-9\]+.D. == b_\[0-9\]+.D.|b_\[0-9\]+.D. == a_\[0-9\]+.D." "optimized" } } */

_Bool lteq_eq(int x, int y)
{
  _Bool c = x < y;
  _Bool d = x == y;
  return c == d; // x > y
}
/* { dg-final { scan-tree-dump "x_\[0-9\]+.D. > y_\[0-9\]+.D.|y_\[0-9\]+.D. < x_\[0-9\]+.D." "optimized" } } */

_Bool ltne_eq(int z, int w)
{
  _Bool c = z < w;
  _Bool d = z != w;
  return c == d; // z <= w
}
/* { dg-final { scan-tree-dump "z_\[0-9\]+.D. <= w_\[0-9\]+.D.|w_\[0-9\]+.D. >= y_\[0-9\]+.D." "optimized" } } */

_Bool lege_eq(int i, int j)
{
  _Bool c = i <= j;
  _Bool d = i >= j;
  return c == d; // i == j
}
/* { dg-final { scan-tree-dump "i_\[0-9\]+.D. == j_\[0-9\]+.D.|j_\[0-9\]+.D. == i_\[0-9\]+.D." "optimized" } } */

_Bool leeq_eq(int k, int l)
{
  _Bool c = k <= l;
  _Bool d = k == l;
  return c == d; // k >= l
}
/* { dg-final { scan-tree-dump "k_\[0-9\]+.D. >= l_\[0-9\]+.D.|l_\[0-9\]+.D. <= k_\[0-9\]+.D." "optimized" } } */

_Bool lene_eq(int m, int n)
{
  _Bool c = m <= n;
  _Bool d = m != n;
  return c == d; // m < n
}
/* { dg-final { scan-tree-dump "m_\[0-9\]+.D. < n_\[0-9\]+.D.|n_\[0-9\]+.D. > m_\[0-9\]+.D." "optimized" } } */
