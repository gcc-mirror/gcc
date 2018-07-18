/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

struct f {}
static int a, b; /* { dg-error "expected ';', identifier or " } */

int f()
{
	return a - b; /* { dg-bogus "invalid operands " } */
}
