/* PR c/102989 */
/* { dg-do compile { target { bitint && dfp } } } */
/* { dg-options "-O2 -std=c23 -pedantic-errors" } */

#if __BITINT_MAXWIDTH__ >= 129
void
foo (_BitInt(129) *x, _Decimal64 *y)
{
  x[0] = y[0];
  y[1] = x[1];
}
#endif

/* _Decimal* <-> _BitInt conversions are unsupported for now.  */
/* { dg-prune-output "unsupported conversion between" } */
