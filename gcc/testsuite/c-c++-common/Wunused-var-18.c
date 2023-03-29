/* PR c/108079 */
/* { dg-do compile } */
/* { dg-options "-Wunused-variable" } */

int
main ()
{
  static int x;	/* { dg-warning "unused variable 'x'" } */
		/* { dg-bogus "'x' defined but not used" "" { target *-*-* } .-1 } */
}
