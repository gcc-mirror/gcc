/* { dg-do compile } */
/* Phi-OPT should be able to optimize this without sinking being invoked. */
/* { dg-options "-O -fdump-tree-phiopt2 -fdump-tree-optimized -fno-tree-sink" } */

int f(int a, int b, int c) {
  int d = a + b;
  if (c > 5) return c;
  if (a == 0) return b;
  return d;
}

unsigned rot(unsigned x, int n) {
  const int bits = __CHAR_BIT__ * __SIZEOF_INT__;
  int t = ((x << n) | (x >> (bits - n)));
  return (n == 0) ? x : t;
}

/* { dg-final { scan-tree-dump-times "goto" 2 "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "goto" 2 "optimized" } } */
