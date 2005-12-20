/* Test the tester; previously gcc.misc-tests/dg-8.c.  */
/* { dg-prms-id 42 } */
/* { dg-options "-Wall" } */
/* { dg-do compile } */
/* { dg-excess-errors "excess errors" } */

/* should get warning about defaulting return type - excess error */

main () { return 0; }
