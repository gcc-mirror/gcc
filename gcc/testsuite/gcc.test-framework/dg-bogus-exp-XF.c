/* Test the tester; previously part of gcc.misc-tests/dg-9.c.  */
/* { dg-prms-id 42 } */
/* { dg-options "-Wall" } */

main (int argc, char *argv[]) {	/* { dg-bogus "return type" "bogus pass test" { xfail *-*-* } } */
}

/* { dg-excess-errors "bogus pass test" { xfail *-*-* } } */
