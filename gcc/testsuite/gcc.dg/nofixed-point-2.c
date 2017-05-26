/* PR inline-asm/39059 */
/* { dg-do compile { target {! fixed_point} } } */
/* { dg-options "-std=gnu99" } */

void
f1 (void)
{
  asm ("" : : "r" (0r));       /* { dg-error "not supported" "reject fixed-point" } */
}

__typeof (0r)		       /* { dg-error "not supported" "reject fixed-point" } */
b2 (void)		       /* { dg-bogus "defaults to" } */
{
  return 0r;		       /* { dg-error "not supported" "reject fixed-point" } */
}

_Accum			       /* { dg-error "not supported" "reject fixed-point" } */
f3 (void)
{
  return 0k;			/* { dg-error "not supported" "reject fixed-point" } */
}

_Sat
/* { dg-error "not supported" "reject fixed-point" { target *-*-* } .-1 } */
/* { dg-error "is used without" "" { target *-*-* } .-2 } */
f4 (void)
{
  return 0k;			/* { dg-error "not supported" "reject fixed-point" } */
}
