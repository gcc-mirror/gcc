/* PR tree-optimization/127879 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fexec-charset=UTF-8" } */
/* { dg-final { scan-tree-dump-times "PHI <(?:-1\\\(\[0-9]*\\\), 1|1\\\(\[0-9]*\\\), -1)\\\(\[0-9]*\\\)>" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not " = a\\\[" "optimized" } } */
/* { dg-final { scan-tree-dump-not " = b.b\\\[" "optimized" } } */

const unsigned char a[] = " !0123456789\0{}~";
const struct { int a; unsigned char b[]; } b = { 0, " !0123456789\0{}~" };

int
foo (int x)
{
  if (x < 2 || x >= 12)
    return -1;
  if (a[x] >= '0' && a[x] <= '9')
    return 1;
  return 0;
}

int
bar (int x)
{
  if (x < 2 || x >= 12)
    return -1;
  if (b.b[x] >= '0' && b.b[x] <= '9')
    return 1;
  return 0;
}
