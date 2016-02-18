/* PR c/69768 */
/* { dg-do compile } */
/* { dg-options "-Waddress" } */

static int e;

int
foo ()
{
  return "foo1" != (void *) 0	/* { dg-bogus "comparison with string literal results in unspecified behavior" } */
	 && "foo2" != (const char *) ((void *) 0)	/* { dg-bogus "comparison with string literal results in unspecified behavior" } */
	 && "foo3" != (const char *) ((void *) (10 - 10))	/* { dg-bogus "comparison with string literal results in unspecified behavior" } */
	 && "foo4" != (const char *) ((void *) (&e - &e))	/* { dg-warning "comparison with string literal results in unspecified behavior" "" { target c } } */
	 && "foo5" != "foo6";	/* { dg-warning "comparison with string literal results in unspecified behavior" } */
}
