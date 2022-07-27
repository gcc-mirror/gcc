/* PR c/105635 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

void foo (int, int[*]);	/* { dg-message "previous declaration of 'foo' with type" } */

foo (int x, int y)	/* { dg-warning "return type defaults to 'int'" } */
{			/* { dg-warning "conflicting types for 'foo'" "" { target *-*-* } .-1 } */
			/* { dg-message "declared here" "" { target *-*-* } .-2 } */
  return (x >= 0) != (y < 0);	/* { dg-warning "'return' with a value, in function returning void" } */
}
