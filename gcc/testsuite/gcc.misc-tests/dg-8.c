/* Test the tester.  */
/* { dg-prms-id 42 } */
/* { dg-options "-Wall" } */
/* { dg-do run } */
/* { dg-excess-errors "excess errors" } */

/* should get warning about defaulting return type - excess error */

main () { return 0; }
