/* PR tree-optimization/113102 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

_BitInt(3) a;
#if __BITINT_MAXWIDTH__ >= 4097
_BitInt(8) b;
_BitInt(495) c;
_BitInt(513) d;
_BitInt(1085) e;
_BitInt(4096) f;

void
foo (void)
{
  a -= (_BitInt(4097)) d >> b;
}

void
bar (void)
{
  __builtin_sub_overflow ((_BitInt(767)) c >> e, 0, &a);
}

void
baz (void)
{
  _BitInt(768) x = (_BitInt(257))f;
  b /= x >> 0 / 0;	/* { dg-warning "division by zero" } */
}
#endif
