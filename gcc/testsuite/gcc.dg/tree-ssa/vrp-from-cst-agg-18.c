/* PR tree-optimization/127879 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fexec-charset=UTF-8" } */
/* { dg-final { scan-tree-dump " = a\\\[" "optimized" } } */
/* { dg-final { scan-tree-dump " = b.b\\\[" "optimized" } } */

const unsigned char a[] = " !012345+789\0{}~";
const struct { int a; unsigned char b[]; } b = { 0, " !01234567+9\0{}~" };

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
