/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/117776 */

/* These 2 functions should be optimized to just `return 1;`
   as `(int)(unsigned smaller)medium` is the same as
   `(int)medium&smaller_mask` where smaller_mask in this case is
   either `1` (_Bool) or `0xff` (char).   */
int f(unsigned char a)
{
  _Bool odd = (a & 1) != 0;
  int odd1 = (a & 1) != 0;
  return odd == odd1;
}
int g(unsigned short a)
{
  unsigned char b = a;
  int d = b;
  unsigned short c = a&0xff;
  int e = c;
  return d == e;
}

/* { dg-final { scan-tree-dump-times "return 1" 2 "optimized" } } */
