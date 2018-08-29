/* PR tree-optimization/86401 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "r\[<>]\[<>]" 2 "optimized" } } */

unsigned int
f1 (unsigned int x, unsigned int s)
{
  unsigned int t = s % (__CHAR_BIT__ * __SIZEOF_INT__);
  return (x << t) | (x >> (((__CHAR_BIT__ * __SIZEOF_INT__) - t) % (__CHAR_BIT__ * __SIZEOF_INT__)));
}

unsigned int
f2 (unsigned int x, unsigned int s)
{
  int n = __CHAR_BIT__ * __SIZEOF_INT__;
  unsigned int t = s % n;
  return (x << t) | (x >> ((n - t) % n));
}
