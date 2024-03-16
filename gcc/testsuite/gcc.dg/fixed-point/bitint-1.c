/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23" } */

void
foo (void)
{
  _Sat _BitInt (42) a;		/* { dg-error "both '_Sat' and '_BitInt' in declaration specifiers" } */
  _BitInt (42) _Sat b;		/* { dg-error "both '_Sat' and '_BitInt' in declaration specifiers" } */
}
