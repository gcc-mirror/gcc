/* PR tree-optimization/112880 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

#if __BITINT_MAXWIDTH__ >= 1024
_BitInt(1024) a, b, c, d, e, f;

void
foo (void)
{
  __builtin_add_overflow (a, b, &a);
  __builtin_sub_overflow (c, d, &c);
  __builtin_mul_overflow (e, f, &e);
}
#endif

#if __BITINT_MAXWIDTH__ >= 512
_BitInt(512) g, h, i, j, k, l;

void
bar (void)
{
  __builtin_add_overflow (g, h, &g);
  __builtin_sub_overflow (i, j, &i);
  __builtin_mul_overflow (k, l, &k);
}
#endif

_BitInt(32) m, n, o, p, q, r;

void
baz (void)
{
  __builtin_add_overflow (m, n, &m);
  __builtin_sub_overflow (o, p, &o);
  __builtin_mul_overflow (q, r, &q);
}
