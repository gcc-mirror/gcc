/* Test the tester; previously part of gcc.misc-tests/dg-9.c.  */
/* { dg-prms-id 42 } */
/* { dg-options "-Wall" } */

f ()
{	/* { dg-error "return type" "warning test" } */
}	/* { dg-error "control reaches end" "warning test" } */

main (int argc, char *argv[])
{		/* { dg-error "return type" "warning test" } */
}		/* { dg-error "control reaches end" "warning test" } */
