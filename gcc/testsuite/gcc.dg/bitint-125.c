/* PR tree-optimization/121127 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -w" } */

#if __BITINT_MAXWIDTH__ >= 576
_BitInt(575)
foo (void)
{
  _BitInt(576) d;
  _BitInt(575) e = d * 42wb;
  return e;
}
#else
int i;
#endif
