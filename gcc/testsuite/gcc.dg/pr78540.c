/* PR middle-end/78540 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -Wno-psabi" } */
/* { dg-additional-options "-march=core2" { target i?86-*-* x86_64-*-* } } */

typedef unsigned __int128 V __attribute__ ((vector_size (64)));
V g;

static inline V
foo (V a)
{
  V b, c;
  c[0] = 0;
  a += 2281559097;
  c ^= 0;
  b[0] = 0;
  return 1 + c + b + a;
}

V
bar ()
{
  V a = g, b = g;
  a[1] ^= 1;
  b[foo (a)[0] & 3] |= 1;
  return b;
}
