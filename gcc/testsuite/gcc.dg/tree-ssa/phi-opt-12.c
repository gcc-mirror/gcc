/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt1" } */

int f(int a, int b, int c) {
  if (c > 5) return c;
  if (a == 0) return b;
  return a + b;
}

unsigned rot(unsigned x, int n) {
  const int bits = __CHAR_BIT__ * __SIZEOF_INT__;
  return (n == 0) ? x : ((x << n) | (x >> (bits - n)));
}

unsigned m(unsigned a, unsigned b) {
  if (a == 0)
    return 0;
  else
    return a & b;
}

/* { dg-final { scan-tree-dump-times "goto" 2 "phiopt1" } } */
/* { dg-final { cleanup-tree-dump "phiopt1" } } */
