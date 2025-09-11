/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=gnu23" } */

void
foo (void)
{
  _Sat _BitInt (42) a;		/* { dg-error "both '_Sat' and '_BitInt' in declaration specifiers" } */
				/* { dg-error "'_Sat' is used without '_Fract' or '_Accum'" "" { target *-*-* } .-1 } */
  _BitInt (42) _Sat b;		/* { dg-error "both '_Sat' and '_BitInt' in declaration specifiers" } */
}
