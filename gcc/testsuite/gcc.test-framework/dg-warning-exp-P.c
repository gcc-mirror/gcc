/* Test the tester; previously part of gcc.misc-tests/dg-9.c.  */
/* { dg-prms-id 42 } */
/* { dg-options "-Wall" } */

int i;

f ()
{	/* { dg-warning "return type" "warning test" } */
  i = 1;
}	/* { dg-warning "control reaches end" "warning test" } */

main (int argc, char *argv[])
{		/* { dg-warning "return type" "warning test" } */
  i = 1;
}		/* { dg-warning "control reaches end" "warning test" } */
