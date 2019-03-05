/* PR rtl-optimization/85770 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -march=nano-1000 -fnon-call-exceptions -fno-tree-coalesce-vars" } */

unsigned a, b, c, d, e, f, g, h, i;
unsigned __int128 j;

__int128 foo(char k, unsigned short l, unsigned m, unsigned n, __int128 o,
             unsigned char p) {
  long q;
  p |= -k;
  __builtin_add_overflow(p, m, &q);
  m *= ~__builtin_clrsbll(0);
  j = j >> (o & 127) | j << (o & 7);
  return k + l + m + n + o + a + b + c + d + j + l + e + f + q + 4294967295 +
         p + g + h + i;
}
