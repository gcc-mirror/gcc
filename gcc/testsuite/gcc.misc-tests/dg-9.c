/* Test the tester.  */
/* { dg-prms-id 42 } */
/* { dg-options "-Wall" } */

f ()
{	/* { dg-warning "return type" "warning test" } */
}	/* { dg-warning "control reaches end" "warning test" } */

main (int argc, char *argv[])
{		/* { dg-warning "return type" "warning test" } */
  +;		/* { dg-error "parse" "error test" } */
		/* { dg-bogus "foobar" "bogus fail test" } */

  return a;	/* { dg-bogus "undeclared|identifier|appears" "bogus pass test" { xfail *-*-* } } */
}		/* { dg-warning "control reaches end" "warning test" } */
