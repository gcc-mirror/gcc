/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23 -pedantic-errors" } */

_BitInt(15) a;
_BitInt(42) b;
#if __BITINT_MAXWIDTH__ >= 115
_BitInt(115) c;
#endif
#if __BITINT_MAXWIDTH__ >= 192
_BitInt(192) d;
#endif
#if __BITINT_MAXWIDTH__ >= 575
_BitInt(575) e;
#endif

int
main ()
{
  __builtin_clear_padding (&a);
  __builtin_clear_padding (&b);
#if __BITINT_MAXWIDTH__ >= 115
  __builtin_clear_padding (&c);
#endif
#if __BITINT_MAXWIDTH__ >= 192
  __builtin_clear_padding (&d);
#endif
#if __BITINT_MAXWIDTH__ >= 575
  __builtin_clear_padding (&e);
#endif
}
