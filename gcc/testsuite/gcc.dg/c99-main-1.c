/* Test for implicit return 0 from main in C99.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors -Wreturn-type -O -fhosted" } */

int
main (void)
{
} /* { dg-bogus "control reaches end" "missing implicit return" } */
