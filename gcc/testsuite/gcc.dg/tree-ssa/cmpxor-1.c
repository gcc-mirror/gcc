/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/107881 */

_Bool ltgtxor(int a, int b)
{
  _Bool c = a < b;
  _Bool d = a > b;
  return c ^ d; // a != b
}
/* { dg-final { scan-tree-dump "a_\[0-9\]+.D. != b_\[0-9\]+.D.|b_\[0-9\]+.D. != a_\[0-9\]+.D." "optimized" } } */

_Bool lteqxor(int x, int y)
{
  _Bool c = x < y;
  _Bool d = x == y;
  return c ^ d; // x <= y (basically | here)
}
/* { dg-final { scan-tree-dump "x_\[0-9\]+.D. <= y_\[0-9\]+.D.|y_\[0-9\]+.D. >= x_\[0-9\]+.D." "optimized" } } */

_Bool ltnexor(int z, int w)
{
  _Bool c = z < w;
  _Bool d = z != w;
  return c ^ d; // z > w
}
/* { dg-final { scan-tree-dump "z_\[0-9\]+.D. > w_\[0-9\]+.D.|w_\[0-9\]+.D. < y_\[0-9\]+.D." "optimized" } } */

_Bool legexor(int i, int j)
{
  _Bool c = i <= j;
  _Bool d = i >= j;
  return c ^ d; // i != j
}
/* { dg-final { scan-tree-dump "i_\[0-9\]+.D. != j_\[0-9\]+.D.|j_\[0-9\]+.D. != i_\[0-9\]+.D." "optimized" } } */

_Bool leeqxor(int k, int l)
{
  _Bool c = k <= l;
  _Bool d = k == l;
  return c ^ d; // k < l
}
/* { dg-final { scan-tree-dump "k_\[0-9\]+.D. < l_\[0-9\]+.D.|l_\[0-9\]+.D. > k_\[0-9\]+.D." "optimized" } } */

_Bool lenexor(int m, int n)
{
  _Bool c = m <= n;
  _Bool d = m != n;
  return c ^ d; // m >= n
}
/* { dg-final { scan-tree-dump "m_\[0-9\]+.D. >= n_\[0-9\]+.D.|n_\[0-9\]+.D. <= m_\[0-9\]+.D." "optimized" } } */
