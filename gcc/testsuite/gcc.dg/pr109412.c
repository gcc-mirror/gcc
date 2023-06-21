/* PR c/107682 */
/* PR c/109412 */
/* { dg-do compile } */
/* { dg-options "" } */

char bar () = {};	/* { dg-error "function 'bar' is initialized like a variable" } */
			/* { dg-error "invalid initializer" "" { target *-*-* } .-1 } */
			/* { dg-message "near initialization for 'bar'" "" { target *-*-* } .-2 } */
char baz () = { 1 };	/* { dg-error "function 'baz' is initialized like a variable" } */
			/* { dg-error "invalid initializer" "" { target *-*-* } .-1 } */
			/* { dg-message "near initialization for 'baz'" "" { target *-*-* } .-2 } */
void
foo ()
{
  int qux () = {};	/* { dg-error "function 'qux' is initialized like a variable" } */
			/* { dg-error "invalid initializer" "" { target *-*-* } .-1 } */
			/* { dg-message "near initialization for 'qux'" "" { target *-*-* } .-2 } */
  int corge () = { 1 };	/* { dg-error "function 'corge' is initialized like a variable" } */
			/* { dg-error "invalid initializer" "" { target *-*-* } .-1 } */
}			/* { dg-message "near initialization for 'corge'" "" { target *-*-* } .-2 } */
