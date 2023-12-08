/* PR tree-optimization/112901 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

float f;
#if __BITINT_MAXWIDTH__ >= 256
_BitInt(256) i;

void
foo (void)
{
  f *= 4 * i;
}
#endif
