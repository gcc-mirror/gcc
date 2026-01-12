/* PR rtl-optimization/123523 */
/* { dg-do compile } */
/* { dg-options "-O -mavx512vl -mavx512bw" } */

typedef __attribute__((__vector_size__ (16))) short V;
typedef __attribute__((__vector_size__ (32))) short W;

char c;
W *p, *q;
short s;

void
bar (V v, int, int, int, int, int, int, void *)
{
  W w = __builtin_ia32_psrlw256_mask ((W) { }, v, *p, 0);
  short x = __builtin_ia32_pcmpgtw256_mask (w, *q, 0);
  __builtin_mul_overflow (x, c, &s);
}

void
foo ()
{
  bar ((V){0, -14}, 0, 0, 0, 0, 0, 0, q);
}
