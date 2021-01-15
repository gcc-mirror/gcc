/* PR tree-optimization/96671 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " \\^ " 6 "optimized" } } */
/* { dg-final { scan-tree-dump-not " ~" "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\| " 6 "optimized" } } */

int
foo (int a, int b, int c)
{
  return (a ^ b) | ((b ^ c) ^ a);
}

int
bar (int a, int b, int c)
{
  return (a ^ b) | ((b ^ a) ^ c);
}

int
baz (int a, int b, int c)
{
  return (a ^ b) | ((a ^ c) ^ b);
}

int
qux (int a, int b, int c)
{
  int d = a ^ b;
  int e = b ^ c;
  int f = e ^ a;
  return d | f;
}

int
corge (int a, int b, int c)
{
  int d = a ^ b;
  int e = b ^ a;
  int f = c ^ e;
  return d | f;
}

int
garply (int a, int b, int c)
{
  int d = a ^ b;
  int e = a ^ c;
  int f = b ^ e;
  return d | f;
}
