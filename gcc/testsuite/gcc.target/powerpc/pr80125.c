/* PR target/80125 */
/* { dg-do compile } */
/* { dg-options "-O2 -maltivec" } */

#include <altivec.h>

int a[1];

void
foo ()
{
  vector int b, e, f, g, h, j, n;
  vector unsigned c, d;
  f = vec_sums (h, b);
  vector int i = vec_mergel (f, g);
  vector int k = vec_mergel (i, j);
  vector int l = vec_sl (k, c);
  vector int m = vec_sl (l, d);
  vector unsigned char o;
  vector int p = vec_perm (m, n, o);
  e = vec_sra (p, c);
  vec_st (e, 0, a);
}
